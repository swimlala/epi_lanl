CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170853  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               	A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؃�U���1   @؃��G @7)��l�D�c�n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    	A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�&�D�Z�D���D��=D�"=D�R�D���D��D��D�_\D��
D���D��D�X�Dڊ�D��)D�#�D�T)D�D�Ф111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B�#�B��>B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�C�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)޸C+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm޸Co޸Cq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP�zDP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW��DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�zDy��D�%�D�Y�D���D��GD�!GD�Q�D���D��)D��D�^fD��D���D��D�W�Dډ�D��3D�"�D�S3D�D�Ϯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A�$�A�+A�+A�+A�-A�+AԲ-Aӝ�A�S�A�33A��A�oA�A���A��A��`A��;A��
A���AҺ^AҰ!Aҧ�Aң�Aқ�AғuA҃A�v�A�-A�;dA�5?A�&�A˼jA���AËDA��-A�bA��A�5?A���A�n�A��mA��A�&�A�VA�1A��A��\A�p�A�dZA�1'A�33A�A�jA�VA�?}A�z�A�A�dZA�=qA�$�A���A��wA��7A���A���A�VA���A�(�A��A�;dA��A�ffA�+A���A�?}A��mA�-A��9A�`BA��A�\)A��A��PA���A���A��A�A���A���A��A��uA���A���A�\)A��7A�S�A��A��wAp�A}hsA|�DA{C�AzM�Ay��Axz�Aw�-AvȴAu�Aq�
Ap��Ao�PAn1'Am\)AlZAkS�Aj5?Ah��Ah(�Af��Afn�Ae�#AehsAd�+AcG�Ab��Aa�FAap�AaVA`{A_�A\ȴA[�AZE�AY��AV�RAT�AR��AO�^ANJAM�AL-AK�TAKdZAJ��AI�AG�wAE/AD��AD�RAD�DAD �AC�AB�9AB1AA�PA@��A@VA?��A>�A<�/A<A�A:��A9�A8��A7��A6~�A3�A333A2��A25?A1��A0�HA/�PA.�+A.1'A-A+�A+�A* �A(�+A(M�A((�A'�FA&��A%��A%�A$�9A#��A#\)A"�yA!��A!VA r�A A�A {A��A�#AZAG�A��AjA��A/AM�A�A�A�/A\)A��A��Ax�AdZA�`A^5AS�A
Q�A	�TA	A^5At�A��A=qA\)A=qAl�A%AĜA �A ȴ@��R@�bN@��#@��@���@��@� �@�J@�r�@�~�@��`@��@�@�p�@�A�@�{@���@��@�~�@�?}@�C�@�n�@���@�+@�?}@ԋD@�A�@�  @���@���@ӥ�@�|�@���@�r�@ϝ�@���@�^5@�@͉7@��@���@̓u@�I�@�b@˝�@�o@�V@�r�@�ȴ@ũ�@��@�  @�@��@��@�b@��@�M�@��@��@�ƨ@�\)@�
=@���@��H@���@���@��\@�x�@��@���@���@�\)@��H@�5?@�`B@���@��
@��@�\)@��!@�E�@�/@��;@�\)@��!@�-@��-@��@�bN@��w@�33@��@�E�@�p�@��j@� �@�t�@��+@�{@���@�`B@�(�@��
@���@�J@�G�@�r�@�I�@��@���@�+@�K�@�l�@�dZ@�
=@�M�@��#@��h@�G�@���@� �@�ƨ@��F@���@���@���@���@�n�@�$�@�$�@�M�@�^5@�@���@�?}@��j@��@��@��F@��y@���@�C�@�C�@�
=@��H@��@���@���@���@�ff@�=q@��@�@���@�$�@��^@���@��`@�Ĝ@�Z@�I�@��;@�S�@�;d@�l�@�C�@��@�~�@�ff@�ff@��\@�n�@��\@���@�V@�J@�@��#@�`B@��@���@��u@�z�@�1'@� �@�b@��@�ƨ@��@���@�l�@�;d@��@�@��y@���@���@���@�ff@�M�@�$�@��@�@�X@��D@��w@�\)@���@�^5@�$�@��-@��@�O�@�p�@�p�@�`B@�O�@�7L@�7L@�/@�/@�/@�/@�/@�/@�&�@�%@���@���@�z�@�Q�@�I�@�(�@��;@�t�@�l�@�\)@�K�@�C�@�;d@�+@�o@���@��@���@���@��\@�-@�=q@�=q@�5?@��@��@���@�A�@خ@w� @o1�@f�r@^�@VW�@N8�@GZ�@A�n@;4�@6��@/��@*��@%+�@ ֡@�@X�@�b@)_@1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A��A�$�A�+A�+A�+A�-A�+AԲ-Aӝ�A�S�A�33A��A�oA�A���A��A��`A��;A��
A���AҺ^AҰ!Aҧ�Aң�Aқ�AғuA҃A�v�A�-A�;dA�5?A�&�A˼jA���AËDA��-A�bA��A�5?A���A�n�A��mA��A�&�A�VA�1A��A��\A�p�A�dZA�1'A�33A�A�jA�VA�?}A�z�A�A�dZA�=qA�$�A���A��wA��7A���A���A�VA���A�(�A��A�;dA��A�ffA�+A���A�?}A��mA�-A��9A�`BA��A�\)A��A��PA���A���A��A�A���A���A��A��uA���A���A�\)A��7A�S�A��A��wAp�A}hsA|�DA{C�AzM�Ay��Axz�Aw�-AvȴAu�Aq�
Ap��Ao�PAn1'Am\)AlZAkS�Aj5?Ah��Ah(�Af��Afn�Ae�#AehsAd�+AcG�Ab��Aa�FAap�AaVA`{A_�A\ȴA[�AZE�AY��AV�RAT�AR��AO�^ANJAM�AL-AK�TAKdZAJ��AI�AG�wAE/AD��AD�RAD�DAD �AC�AB�9AB1AA�PA@��A@VA?��A>�A<�/A<A�A:��A9�A8��A7��A6~�A3�A333A2��A25?A1��A0�HA/�PA.�+A.1'A-A+�A+�A* �A(�+A(M�A((�A'�FA&��A%��A%�A$�9A#��A#\)A"�yA!��A!VA r�A A�A {A��A�#AZAG�A��AjA��A/AM�A�A�A�/A\)A��A��Ax�AdZA�`A^5AS�A
Q�A	�TA	A^5At�A��A=qA\)A=qAl�A%AĜA �A ȴ@��R@�bN@��#@��@���@��@� �@�J@�r�@�~�@��`@��@�@�p�@�A�@�{@���@��@�~�@�?}@�C�@�n�@���@�+@�?}@ԋD@�A�@�  @���@���@ӥ�@�|�@���@�r�@ϝ�@���@�^5@�@͉7@��@���@̓u@�I�@�b@˝�@�o@�V@�r�@�ȴ@ũ�@��@�  @�@��@��@�b@��@�M�@��@��@�ƨ@�\)@�
=@���@��H@���@���@��\@�x�@��@���@���@�\)@��H@�5?@�`B@���@��
@��@�\)@��!@�E�@�/@��;@�\)@��!@�-@��-@��@�bN@��w@�33@��@�E�@�p�@��j@� �@�t�@��+@�{@���@�`B@�(�@��
@���@�J@�G�@�r�@�I�@��@���@�+@�K�@�l�@�dZ@�
=@�M�@��#@��h@�G�@���@� �@�ƨ@��F@���@���@���@���@�n�@�$�@�$�@�M�@�^5@�@���@�?}@��j@��@��@��F@��y@���@�C�@�C�@�
=@��H@��@���@���@���@�ff@�=q@��@�@���@�$�@��^@���@��`@�Ĝ@�Z@�I�@��;@�S�@�;d@�l�@�C�@��@�~�@�ff@�ff@��\@�n�@��\@���@�V@�J@�@��#@�`B@��@���@��u@�z�@�1'@� �@�b@��@�ƨ@��@���@�l�@�;d@��@�@��y@���@���@���@�ff@�M�@�$�@��@�@�X@��D@��w@�\)@���@�^5@�$�@��-@��@�O�@�p�@�p�@�`B@�O�@�7L@�7L@�/@�/@�/@�/@�/@�/@�&�@�%@���@���@�z�@�Q�@�I�@�(�@��;@�t�@�l�@�\)@�K�@�C�@�;d@�+@�o@���@��@���@���@��\@�-@�=q@�=q@�5?@��@��G�O�@�A�@خ@w� @o1�@f�r@^�@VW�@N8�@GZ�@A�n@;4�@6��@/��@*��@%+�@ ֡@�@X�@�b@)_@1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBm�Bn�Bn�Bm�Bm�Bm�Bm�Bl�Bm�Bp�B~�B�sBiyBx�Bx�Bw�Bw�Bw�Bw�Bx�By�By�By�By�By�By�Bz�B{�B{�B{�B{�B{�B�B�VB�PB��B�RB�^B��B�B�/B�B��B�}B�LB�dB�?B�9B�'B�!B��B��B��B��B��B��B��B�oB�VB�JBy�Br�Bm�BjBgmBdZB]/B>wB2-B#�B�B
=B  B�B�)B��B�wB�jB�-B�B��B��B�bB� BjB]/BM�B6FB!�BJBB
�B
�NB
��B
��B
��B
ƨB
ŢB
��B
�LB
��B
��B
��B
�1B
v�B
p�B
jB
dZB
`BB
ZB
S�B
M�B
E�B
0!B
%�B
�B
�B
VB
1B	��B	��B	�B	�B	�sB	�`B	�HB	�/B	�B	��B	��B	ȴB	ŢB	ÖB	�qB	�FB	�B	��B	��B	�oB	�%B	s�B	iyB	Q�B	C�B	>wB	9XB	7LB	6FB	6FB	33B	.B	�B	�B	�B	�B	�B	uB	bB	JB	DB		7B	1B	B	B��B��B�B�B�yB�ZB�BB�#B��B��B��B��BǮBB�wB�jB�^B�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�PB�7B�B�B~�By�Bx�Bs�Br�Bk�Bk�BffBe`B`BB`BBYBT�BR�BP�BL�BK�BJ�BG�BF�BE�BB�BB�B@�B?}B>wB=qB<jB=qB:^B9XB7LB7LB49B33B1'B2-B5?B49B5?B49B49B2-B2-B6FB7LB7LB8RB8RB5?B5?B8RB:^B>wB@�B?}B?}BA�BD�BE�BE�BK�BK�BN�BN�BO�BP�BP�BQ�BQ�BR�BR�BR�BR�BS�BT�BW
BYB[#B[#B^5B`BBaHBbNBcTBffBffBffBhsBjBjBk�Bk�Bl�Bl�Bl�Bl�Bo�Bq�Bq�Bq�Bq�Bs�Bt�Bu�Bw�Bx�Bx�By�B{�B{�B�B�B�B�+B�7B�=B�PB�VB�bB�bB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�'B�FB�jB�}BɺB��B��B��B�B�/B�NB�B�B�B�B��B��B��B��B��B	B	%B	
=B	\B	uB	�B	�B	�B	 �B	&�B	)�B	+B	)�B	.B	.B	-B	.B	6FB	;dB	<jB	=qB	=qB	?}B	@�B	@�B	@�B	@�B	D�B	F�B	J�B	N�B	O�B	O�B	O�B	Q�B	O�B	P�B	P�B	Q�B	VB	]/B	`BB	cTB	gmB	jB	l�B	p�B	r�B	u�B	z�B	|�B	~�B	�B	�B	�1B	�7B	�DB	�PB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�3B	�9B	�9B	�9B	�9B	�?B	�?B	�?B	�FB	�LB	�^B	�qB	�wB	��B	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	��B	�B
 �B
DB
sB
�B
(sB
0�B
8�B
>�B
D�B
I�B
OBB
SuB
X�B
^�B
d@B
hsB
lB
o�B
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BdpBewBewBdpBdpBdpBdpBcjBdpBg�Bu�B�IB`GBo�Bo�Bn�Bn�Bn�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Br�Br�Br�Br�Br�Bw�B�!B�B�XB�B�'BȵB��B��B��BǯB�IB�B�1B�B�B��B��B��B��B��B��B��B�wB�kB�AB�(B�Bp�Bi�BdfBaTB^BB[0BTB5PB)B�BiBB��B�^B�
B�fB�ZB�MB�B��B��B�yB�IBv�BaiBTBD�B-4B�B;B
��B
�B
�CB
��B
��B
��B
��B
��B
��B
�DB
��B
��B
��B
-B
m�B
g�B
a~B
[YB
WBB
QB
J�B
D�B
<�B
'%B
�B
�B
�B
\B	�8B	�B	��B	�B	�B	�}B	�jB	�RB	�:B	�"B	��B	��B	��B	��B	��B	�~B	�TB	�B	��B	��B	��B	}7B	j�B	`�B	IB	:�B	5�B	0pB	.dB	-_B	-_B	*LB	%.B	�B	�B	�B	�B	�B	
�B	~B	fB	aB	 TB�NB�<B�/B��B��B��B�B��B�zB�cB�DB�B�B��B��B��B��B��B��B��B�kB�XB�FB�(B�B�B�"B�B�B��B��B��B��B��B��B��B��B��B��B��B�yB�aB|IBy6Bv%BqBp Bj�Bi�Bb�Bb�B]�B\�BWpBWpBPFBL-BJ!BHBC�BB�BA�B>�B=�B<�B9�B9�B7�B6�B5�B4�B3�B4�B1�B0�B.B.B+lB*fB([B)aB,rB+mB,sB+mB+mB)aB)aB-zB.�B.�B/�B/�B,tB,tB/�B1�B5�B7�B6�B6�B8�B;�B<�B<�BB�BB�BFBFBGBHBHBI BI BJ&BJ&BJ&BJ&BK,BL2BN>BPKBRWBRWBUiBWvBX|BY�BZ�B]�B]�B]�B_�Ba�Ba�Bb�Bb�Bc�Bc�Bc�Bc�Bf�Bh�Bh�Bh�Bh�Bj�Bk�Bl�BoBpBpBqBsBsBx8B|QB{KB~]B�iB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�DB�WB�vB��B��B��B�B�B�&B�EB�\B�{B�B��B��B��B��B��B�B�B�B�1B�PB	hB	�B	
�B	�B	�B	�B	�B	B	!%B	"+B	!%B	%=B	%=B	$7B	%=B	-nB	2�B	3�B	4�B	4�B	6�B	7�B	7�B	7�B	7�B	;�B	=�B	A�B	E�B	GB	GB	GB	IB	GB	HB	HB	IB	M*B	TUB	WhB	ZyB	^�B	a�B	c�B	g�B	i�B	l�B	rB	tB	vB	x)B	z5B	TB	�ZB	�gB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�/B	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�_B	�_B	�_B	�fB	�lB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�(B	�.B	�.B	�5B	�AG�O�B	�	B	��B	�B
`B
�B
�B
�B
'�B
/�B
5�B
;�B
A	B
F\B
J�B
O�B
U�B
[YB
_�B
c!B
f�B
k111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200619170853    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170853  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170853  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                