CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:56Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170856  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؈�F��1   @؈��8�@7��t�j�c�M���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D-��D.� D/  D/� D0  D0�fD1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�D� �D�M�D��
D�ڏD�,)D�VD���D��)D�=D�W\D���D�� D�3D�[�Dړ�D�� D��D�UD� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B�W
B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC(�C)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#��D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,�zD,�D-~D-��D.~D.�D/~D/�D0�zD1zD1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DSzDS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp��Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�GDy�(D��D�L�D��D�ٙD�+3D�UD���D��3D�GD�VfD���D��
D�=D�Z�Dڒ�D��
D��D�T)D�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A���A�^5A�VA��A��A���A���A���AԺ^AԸRA԰!Aԩ�Aԥ�Aԧ�Aԣ�AԬAԬAԧ�Aԥ�Aԩ�Aԩ�Aԗ�A�p�A�oA�9XAҴ9A��A��HA�A�A�bA�Q�AʑhA�dZA��A�p�A���A�?}A��+A��wA��A�p�A�x�A��;A��!A��;A��jA�A�  A�ZA��\A��PA��A�|�A�;dA�&�A��A�E�A���A��A�ZA��!A���A�;dA��hA�$�A���A�z�A�/A�
=A�dZA��uA���A�-A��A�r�A���A�A��uA��yA��TA�VA��
A��;A��A���A�A�E�A���A�JA�9XA��PA���A��A�M�A��yA��\A�-A�A�A��^A�|�A�bNA�C�A�;dAdZA}p�A|�\A{�hAw��AuK�Ast�Aq�TAo��Al��Al�Ak?}Aj�!AihsAhjAfȴAd^5Ac�AbI�A`A]oAZ�AX=qAV�AUx�ATbNAS��AR�`AN�9AK�AJ �AG��AE��AD1'AC��AB�!AB-AA�A>VA<�uA;�A;7LA:�RA9�A85?A77LA6I�A5A4��A2��A1��A1�A0��A0ZA0$�A/�wA/
=A-�mA,jA,bA+�;A+�wA+��A+O�A+
=A*bNA)t�A)7LA(��A(jA(jA'A&n�A%��A$Q�A#�A"bA!dZA!oA ��Az�A��AAG�A(�Al�A^5AȴAbAS�A��A��Al�A�A��A�\AA�A�wAG�A(�A�7A/AVA��AXA��A�TA�A-A
=A	�-AjA�-A��A�jA�9A��AG�A&�A��A��A �/A ^5@�n�@��/@�S�@�v�@��`@�l�@�5?@���@� �@�$�@�S�@���@���@��@�E�@�9X@�@�x�@�j@��@�\)@�@�{@��@���@�E�@�x�@�I�@�r�@���@ڸR@��#@�hs@��@ج@�r�@�Q�@�9X@ם�@ָR@�b@�@Гu@ύP@�~�@��T@�O�@�A�@�K�@��y@�V@�1'@�@�ff@�$�@ũ�@�/@���@�1'@�ƨ@�n�@�7L@�Z@�dZ@��H@�E�@���@���@��w@�;d@���@���@���@���@���@��h@���@�r�@���@�=q@�?}@��@�Q�@��P@�J@�`B@��@���@�I�@�  @�V@�Ĝ@�l�@���@�$�@��h@��@��m@�C�@���@���@��9@�r�@� �@�  @�|�@�v�@�{@�@���@�p�@�O�@���@��u@��j@�r�@��@��m@�t�@�o@���@���@�ff@�-@�{@��@���@��-@�hs@��@��h@�?}@��`@���@��j@���@�bN@�r�@�Q�@� �@�C�@��H@�~�@�$�@���@�@��7@�X@��@��`@���@�Ĝ@��@�I�@�1'@�b@��@��P@��@��P@��P@��P@�;d@�ȴ@���@��\@�V@�^5@�v�@�M�@�p�@��@���@��`@���@��j@�Ĝ@���@���@�Ĝ@��D@�z�@�j@�bN@�(�@��;@�ƨ@��P@�C�@�o@��H@��!@��\@�n�@��h@�-@�{@��@���@�Ĝ@�z�@�Z@�z�@�(�@���@�\)@�"�@���@��\@�~�@�n�@�M�@��@�{@���@��@��T@���@��^@���@��h@��@�p�@�O�@�?}@�&�@���@���@�r�@�j@�Z@�I�@�9X@�(�@�b@�ƨ@�t�@�;d@�@��y@���@���@��\@�E�@���@��#@���@��-@��h@�G�@�V@���@��@�z�@�I�@�1@��P@�C�@�
=@��@���@���@��?@wS@n��@j1�@b6�@W�@O��@JYK@C��@?�@7��@0�@+K�@%;@ �.@��@?�@(�@��@�O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A� �A���A�^5A�VA��A��A���A���A���AԺ^AԸRA԰!Aԩ�Aԥ�Aԧ�Aԣ�AԬAԬAԧ�Aԥ�Aԩ�Aԩ�Aԗ�A�p�A�oA�9XAҴ9A��A��HA�A�A�bA�Q�AʑhA�dZA��A�p�A���A�?}A��+A��wA��A�p�A�x�A��;A��!A��;A��jA�A�  A�ZA��\A��PA��A�|�A�;dA�&�A��A�E�A���A��A�ZA��!A���A�;dA��hA�$�A���A�z�A�/A�
=A�dZA��uA���A�-A��A�r�A���A�A��uA��yA��TA�VA��
A��;A��A���A�A�E�A���A�JA�9XA��PA���A��A�M�A��yA��\A�-A�A�A��^A�|�A�bNA�C�A�;dAdZA}p�A|�\A{�hAw��AuK�Ast�Aq�TAo��Al��Al�Ak?}Aj�!AihsAhjAfȴAd^5Ac�AbI�A`A]oAZ�AX=qAV�AUx�ATbNAS��AR�`AN�9AK�AJ �AG��AE��AD1'AC��AB�!AB-AA�A>VA<�uA;�A;7LA:�RA9�A85?A77LA6I�A5A4��A2��A1��A1�A0��A0ZA0$�A/�wA/
=A-�mA,jA,bA+�;A+�wA+��A+O�A+
=A*bNA)t�A)7LA(��A(jA(jA'A&n�A%��A$Q�A#�A"bA!dZA!oA ��Az�A��AAG�A(�Al�A^5AȴAbAS�A��A��Al�A�A��A�\AA�A�wAG�A(�A�7A/AVA��AXA��A�TA�A-A
=A	�-AjA�-A��A�jA�9A��AG�A&�A��A��A �/A ^5@�n�@��/@�S�@�v�@��`@�l�@�5?@���@� �@�$�@�S�@���@���@��@�E�@�9X@�@�x�@�j@��@�\)@�@�{@��@���@�E�@�x�@�I�@�r�@���@ڸR@��#@�hs@��@ج@�r�@�Q�@�9X@ם�@ָR@�b@�@Гu@ύP@�~�@��T@�O�@�A�@�K�@��y@�V@�1'@�@�ff@�$�@ũ�@�/@���@�1'@�ƨ@�n�@�7L@�Z@�dZ@��H@�E�@���@���@��w@�;d@���@���@���@���@���@��h@���@�r�@���@�=q@�?}@��@�Q�@��P@�J@�`B@��@���@�I�@�  @�V@�Ĝ@�l�@���@�$�@��h@��@��m@�C�@���@���@��9@�r�@� �@�  @�|�@�v�@�{@�@���@�p�@�O�@���@��u@��j@�r�@��@��m@�t�@�o@���@���@�ff@�-@�{@��@���@��-@�hs@��@��h@�?}@��`@���@��j@���@�bN@�r�@�Q�@� �@�C�@��H@�~�@�$�@���@�@��7@�X@��@��`@���@�Ĝ@��@�I�@�1'@�b@��@��P@��@��P@��P@��P@�;d@�ȴ@���@��\@�V@�^5@�v�@�M�@�p�@��@���@��`@���@��j@�Ĝ@���@���@�Ĝ@��D@�z�@�j@�bN@�(�@��;@�ƨ@��P@�C�@�o@��H@��!@��\@�n�@��h@�-@�{@��@���@�Ĝ@�z�@�Z@�z�@�(�@���@�\)@�"�@���@��\@�~�@�n�@�M�@��@�{@���@��@��T@���@��^@���@��h@��@�p�@�O�@�?}@�&�@���@���@�r�@�j@�Z@�I�@�9X@�(�@�b@�ƨ@�t�@�;d@�@��y@���@���@��\@�E�@���@��#@���@��-@��h@�G�@�V@���@��@�z�@�I�@�1@��P@�C�@�
=@��@���G�O�@��?@wS@n��@j1�@b6�@W�@O��@JYK@C��@?�@7��@0�@+K�@%;@ �.@��@?�@(�@��@�O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�?B�3B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�-B�-B�-B�-B�3B�FB�wB��B��B33BA�BJ�BO�BXBYBe`Bt�BjBe`Bp�Bs�By�By�B�+B�PB�hB�bB��B��B�uB�oB�VB�1B�+B�B~�B{�Bt�Bq�Bm�BgmBe`BaHB^5BS�BM�B?}B9XB.B&�B"�B�BVB��B�B�BB��BƨB�qB��B��B�oB�=B}�Bp�B`BBZBM�B;dB6FB,B�B%B
�B
�#B
�}B
�B
��B
�JB
t�B
n�B
]/B
>wB
33B
.B
-B
)�B
'�B
 �B
uB
	7B
  B	�sB	��B	ŢB	�dB	��B	��B	�\B	�7B	�B	z�B	s�B	k�B	ZB	N�B	I�B	;dB	,B	�B	PB��B��B�B�yB�ZB��B�LB�B��B��B�hB�\B�PB�JB�DB��B�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�hB�\B�=B�7B�1B�1B�+B�+B�B�B� B~�B|�B{�Bz�Bz�Bz�By�Bu�Bs�Bq�Bp�Bo�Bm�BjBgmBffBdZBbNBbNBe`B^5BdZBaHB`BB[#BYBXBW
BYB_;BbNBaHBaHB`BB_;B]/B[#BYBYBW
BR�BR�BT�BP�BJ�BE�BH�BM�BD�B>wB?}B>wB>wB>wB<jB<jB<jB<jB<jB;dB;dB:^B9XB6FB5?B5?B7LB6FB6FB5?B8RB8RB8RB8RB8RB9XB9XB:^B<jB;dB:^B9XB:^B?}BC�BE�BF�BG�BF�BF�BH�BI�BI�BI�BJ�BK�BL�BJ�BH�BI�BJ�BI�BL�BN�BP�BQ�BW
BW
BZB[#B[#B\)B]/B]/B^5B^5B`BB`BB`BBaHBaHBbNBcTBe`BgmBgmBjBl�Bp�Br�Bt�Bv�Bw�Bw�By�B|�B~�B� B�B�B�1B�7B�=B�DB�JB�JB�hB��B��B��B��B��B��B�B�B�-B�FB�wB��BÖBÖBƨB��B��B��B��B�
B�B�NB�ZB�mB�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	
=B	�B	�B	�B	�B	 �B	#�B	'�B	+B	.B	.B	5?B	:^B	>wB	E�B	I�B	J�B	K�B	L�B	N�B	Q�B	T�B	T�B	YB	_;B	`BB	aHB	e`B	ffB	ffB	ffB	ffB	ffB	hsB	hsB	hsB	iyB	jB	k�B	l�B	m�B	p�B	q�B	s�B	t�B	u�B	w�B	|�B	~�B	� B	�B	�B	�+B	�7B	�=B	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�FB	�RB	�XB	�dB	�jB	�jB	�qB	�qB	�wB	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�`B	�fB	�sB	�sB	�B	�B	��B
'B
JB
NB
kB
%,B
/5B
6zB
<PB
AB
HfB
N�B
SuB
YB
_�B
c�B
h$B
lB
p!B
q'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B�OB�CB�,B�B�B�B�B�B�B�B�B�B�B�B�,B�2B�>B�>B�>B�>B�DB�VB��B�B�B*;B8�BA�BF�BOBPB\eBk�Ba�B\fBg�Bj�Bp�Bp�B~1B�VB�nB�hB��B��B�|B�vB�^B:B~4BzBvBr�Bk�Bh�Bd�B^yB\lBXTBUBBKBD�B6�B0hB%$B�B�B�BiB��B�B�XB��B��B��B�B��B��B�ZBuBg�BWbBQ>BD�B2�B-jB#,B�B
�LB
�B
�NB
��B
�IB
��B
�{B
k�B
e�B
TcB
5�B
*kB
%LB
$FB
!4B
)B
�B

�B
 rB	�<B	߱B	�7B	��B	��B	�3B	��B	��B	�|B	zXB	r'B	j�B	b�B	QfB	F#B	AB	2�B	#UB	�B	�B�JB� B��B��B۬B�LB��B�kB�)B��B��B��B��B��B��B��B��B��B��B��B��B� B�1B�BB�IB�=B�B�B��B��B��B��B��B��B��B��B��B�B�B~�B~�B||B{vBw]BvXBtLBsEBr?Br?Br?Bq9Bm"BkBi	BhBf�Bd�Ba�B^�B]�B[�BY�BY�B\�BU�B[�BX�BW�BR�BPyBOrBNlBPyBV�BY�BX�BX�BW�BV�BT�BR�BPzBPzBNmBJVBJVBLbBHIBB&B=B@BE8B<B5�B6�B5�B5�B5�B3�B3�B3�B3�B3�B2�B2�B1�B0�B-�B,�B,�B.�B-�B-�B,�B/�B/�B/�B/�B/�B0�B0�B1�B3�B2�B1�B0�B1�B6�B:�B=
B>B?B>B>B@BA"BA"BA"BB)BC/BD5BB)B@BA#BB*BA#BD6BFBBHNBITBNrBNrBQ�BR�BR�BS�BT�BT�BU�BU�BW�BW�BW�BX�BX�BY�BZ�B\�B^�B^�Ba�Bc�BhBjBl#Bn0Bo6Bo6BqBBtUBvaBwgBxmB{�B�B��B��B��B��B��B��B��B�B�$B�1B�=B�HB�gB��B��B��B��B��B��B��B�B�6B�BB�HB�[B�mB�BٰBۼB��B��B��B�B�B�B�B�0B�6B�6B�BB�GB�ZB�fB��B	�B	�B	�B	
B	B	#B	5B	NB	"`B	%qB	%qB	,�B	1�B	5�B	<�B	AB	BB	C#B	D(B	F4B	IGB	LYB	LYB	PrB	V�B	W�B	X�B	\�B	]�B	]�B	]�B	]�B	]�B	_�B	_�B	_�B	`�B	a�B	b�B	c�B	d�B	g�B	iB	kB	lB	mB	o(B	tGB	vSB	wXB	ydB	|wB	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�9B	�FB	�?B	�3B	�3B	�?B	�XB	�jB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�3B	�3B	�3B	�3B	�9B	�LB	�WB	�cB	�]B	�]B	�cB	�pB	�pB	�vB	�|B	ՈB	ՈB	֎B	וB	؛B	١B	ڧB	ڧB	ܳB	ݹB	��B	��B	��G�O�B	�KB	�xB
�B
�B
�B
|B
&�B
-�B
3�B
8UB
?�B
FBB
J�B
PfB
V�B
[?B
_rB
cVB
goB
hu11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170856    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170856  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170856  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                