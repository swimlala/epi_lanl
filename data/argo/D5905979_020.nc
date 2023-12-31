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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170856  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؉�σZ1   @؉����@7���v��c̋C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�{D�$�D�H�D��D��D�)�D�^�D���D��\D�&�D�[�D���D��D� RD�V�Dڗ\D��D�%D�W
D��D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
=@�
=A�A?�A_�A�A���A���A�A�A�A�A�A�B�HB�HBG�B�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB��B�W
B��B��B�#�B�#�B��qB��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS޸CU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DQzDQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd��De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�GDy��D�#�D�G�D��D��D�(�D�]�D���D��fD�%�D�Z�D���D��D�\D�U�DږfD��D�$)D�VD� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�z�AՅAՇ+AՇ+AՅAՃAՇ+AՉ7AՇ+AՉ7AՉ7AՇ+AՍPAՏ\AՏ\AՑhAՓuAՓuAՓuAՕ�AՕ�AՕ�A՗�Aՙ�Aՙ�Aՙ�A՗�A՗�AՋDA�(�A�\)A҅AЙ�A�ZA� �A�K�A�  A��-A���A�ZA�9XA��A�=qA�O�A���A��^A�`BA�9XA��#A�VA��+A��^A�G�A��hA��wA�A�O�A���A�  A�G�A���A�A�t�A�A�S�A�t�A�?}A�ĜA�`BA�Q�A�A��hA�ZA��A�1'A�z�A�E�A�$�A��A� �A�C�A��mA�JA��A�+A��A���A�&�A��A� �A��PA�?}A�7LA�  A�  A�%A���A��A��DA��;A�p�A��A���A�r�A���A��A~�A}�AzbAv�HAtffAq��Anz�Alr�Ak�wAk�hAjr�Ag`BAe��AbffAa+A`�+A_S�A]A\5?AZ(�AXn�AW;dAV��AS��AN-AK�AK&�AJ�RAH  AFE�AEoADA�AC�wACA@�`A>Q�A;��A:M�A8E�A7�
A7hsA7/A6��A6�A6ZA6=qA5�-A4�HA3�hA2=qA0�HA0=qA.��A.1'A-�
A-|�A-K�A-;dA,�!A+��A*�A)S�A((�A'�hA'XA'oA&��A%��A$r�A#�;A#O�A"5?A!�wA ~�A9XA�AƨA+A�Ar�Al�A9XA�-A�/AbNA�
A�/AbAl�A�DAK�A�!A�RA�A��A�A&�AO�A�wAhsAVA�TA�A��A
r�A
Q�A��A��A�yA�hA�A�!AbNA?}@��^@���@�33@��+@�@�z�@��@�-@�V@�  @�@��@�=q@��`@��@�7L@�  @�\@�p�@�+@�=q@��@�O�@��
@�E�@�hs@���@�1@ߕ�@�+@�n�@���@�|�@�{@�x�@�7L@�(�@ׅ@�v�@Չ7@�|�@���@�\)@�`B@̋D@�1'@˥�@���@�n�@�@��/@ǍP@�{@ź^@ģ�@Ý�@�$�@�V@�Z@�l�@�@��H@��R@��^@��@�A�@��F@���@���@��@� �@�dZ@�ff@�7L@��@���@���@�p�@��@���@��P@���@���@�%@��D@�1@�\)@��R@���@�V@�9X@�;d@�o@��@���@�-@�x�@��j@��;@���@��F@��@��R@��@�G�@��@��D@�9X@���@�o@�~�@�5?@�J@�@��@��-@��7@�X@�O�@�O�@�?}@�O�@�p�@�hs@�G�@�V@���@�Ĝ@��@�z�@�j@��@��F@�dZ@���@�v�@�^5@�V@�M�@�5?@��@���@�J@�@�@��^@��^@���@�%@���@��@�Ĝ@��D@�z�@�I�@�(�@��@��@��@�l�@�K�@�"�@��@��!@�E�@���@�x�@�`B@�/@��`@�Ĝ@��@�r�@�(�@�  @��F@�
=@�ff@�^5@�M�@�@�@�x�@��@�r�@�9X@� �@�A�@�Z@� �@��
@��F@�t�@�l�@��H@��R@��\@�^5@�n�@�n�@�M�@��@��^@���@���@��@�p�@�O�@��/@���@��@��@�Z@�1'@��@�  @��
@��;@��@���@�K�@�\)@��P@�C�@�@��H@��R@�v�@��\@���@���@���@�~�@�M�@�{@�@��@���@��7@�O�@�/@��@�%@�Ĝ@�j@�Z@�9X@���@��w@��@�l�@�33@��y@�~�@�M�@�-@���@��7@�`B@�/@�V@�Ĝ@��9@���@�A�@���@��F@���@��~@x�?@ru@l1@b�@Z�H@R�R@M*0@H�_@B:*@<A�@3�m@-��@(PH@$	�@ 4n@�@��@�h@�}@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�l�A�z�AՅAՇ+AՇ+AՅAՃAՇ+AՉ7AՇ+AՉ7AՉ7AՇ+AՍPAՏ\AՏ\AՑhAՓuAՓuAՓuAՕ�AՕ�AՕ�A՗�Aՙ�Aՙ�Aՙ�A՗�A՗�AՋDA�(�A�\)A҅AЙ�A�ZA� �A�K�A�  A��-A���A�ZA�9XA��A�=qA�O�A���A��^A�`BA�9XA��#A�VA��+A��^A�G�A��hA��wA�A�O�A���A�  A�G�A���A�A�t�A�A�S�A�t�A�?}A�ĜA�`BA�Q�A�A��hA�ZA��A�1'A�z�A�E�A�$�A��A� �A�C�A��mA�JA��A�+A��A���A�&�A��A� �A��PA�?}A�7LA�  A�  A�%A���A��A��DA��;A�p�A��A���A�r�A���A��A~�A}�AzbAv�HAtffAq��Anz�Alr�Ak�wAk�hAjr�Ag`BAe��AbffAa+A`�+A_S�A]A\5?AZ(�AXn�AW;dAV��AS��AN-AK�AK&�AJ�RAH  AFE�AEoADA�AC�wACA@�`A>Q�A;��A:M�A8E�A7�
A7hsA7/A6��A6�A6ZA6=qA5�-A4�HA3�hA2=qA0�HA0=qA.��A.1'A-�
A-|�A-K�A-;dA,�!A+��A*�A)S�A((�A'�hA'XA'oA&��A%��A$r�A#�;A#O�A"5?A!�wA ~�A9XA�AƨA+A�Ar�Al�A9XA�-A�/AbNA�
A�/AbAl�A�DAK�A�!A�RA�A��A�A&�AO�A�wAhsAVA�TA�A��A
r�A
Q�A��A��A�yA�hA�A�!AbNA?}@��^@���@�33@��+@�@�z�@��@�-@�V@�  @�@��@�=q@��`@��@�7L@�  @�\@�p�@�+@�=q@��@�O�@��
@�E�@�hs@���@�1@ߕ�@�+@�n�@���@�|�@�{@�x�@�7L@�(�@ׅ@�v�@Չ7@�|�@���@�\)@�`B@̋D@�1'@˥�@���@�n�@�@��/@ǍP@�{@ź^@ģ�@Ý�@�$�@�V@�Z@�l�@�@��H@��R@��^@��@�A�@��F@���@���@��@� �@�dZ@�ff@�7L@��@���@���@�p�@��@���@��P@���@���@�%@��D@�1@�\)@��R@���@�V@�9X@�;d@�o@��@���@�-@�x�@��j@��;@���@��F@��@��R@��@�G�@��@��D@�9X@���@�o@�~�@�5?@�J@�@��@��-@��7@�X@�O�@�O�@�?}@�O�@�p�@�hs@�G�@�V@���@�Ĝ@��@�z�@�j@��@��F@�dZ@���@�v�@�^5@�V@�M�@�5?@��@���@�J@�@�@��^@��^@���@�%@���@��@�Ĝ@��D@�z�@�I�@�(�@��@��@��@�l�@�K�@�"�@��@��!@�E�@���@�x�@�`B@�/@��`@�Ĝ@��@�r�@�(�@�  @��F@�
=@�ff@�^5@�M�@�@�@�x�@��@�r�@�9X@� �@�A�@�Z@� �@��
@��F@�t�@�l�@��H@��R@��\@�^5@�n�@�n�@�M�@��@��^@���@���@��@�p�@�O�@��/@���@��@��@�Z@�1'@��@�  @��
@��;@��@���@�K�@�\)@��P@�C�@�@��H@��R@�v�@��\@���@���@���@�~�@�M�@�{@�@��@���@��7@�O�@�/@��@�%@�Ĝ@�j@�Z@�9X@���@��w@��@�l�@�33@��y@�~�@�M�@�-@���@��7@�`B@�/@�V@�Ĝ@��9@���@�A�@���@��FG�O�@��~@x�?@ru@l1@b�@Z�H@R�R@M*0@H�_@B:*@<A�@3�m@-��@(PH@$	�@ 4n@�@��@�h@�}@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�jB�dB�dB�dB�jB�dB�jB�wB��B��B �B8RBA�B^5BjBs�Br�Bv�Bx�B|�B�B�B�PB�VB�uB��B��B��B��B��B��B�{B�uB�oB�bB�PB�DB�B~�Bq�Bm�BgmBbNB]/BS�BE�B;dB6FB,B�B�B�B\B1B��B��B�B�fBȴB�XB�'B��B�{B�%B}�Bs�BdZBZBQ�BG�B49B�BB
�B
�;B
ǮB
�'B
��B
~�B
e`B
VB
M�B
D�B
8RB
.B
!�B
uB
B	�mB	�B	B	�B	��B	�uB	�bB	�DB	u�B	l�B	VB	K�B	F�B	?}B	5?B	)�B	�B	bB	+B	  B�B��BÖB�wB�^B�'B��B��B��B��B�oB�=B|�Bs�Bl�Bn�Bq�Br�Br�Bs�Bu�Bv�Bw�Bx�Bx�Bu�Bt�Bp�Bo�Bo�Bm�Bn�Bm�Bq�Bx�B�B�B~�Bz�By�Bx�Bx�Bv�Bv�Bv�Bq�Bo�Bp�Bo�Bl�BhsBaHB]/BZBZBYBZB]/B\)BXBVBVBT�BP�BP�BR�BQ�BR�BN�BO�BR�BL�BH�BH�BK�BW
BYBVBN�BN�BP�BO�BO�BQ�BM�BK�BJ�BH�BD�BC�B?}B?}B<jB;dB<jB;dB<jB;dB:^B;dB9XB9XB8RB7LB6FB8RB6FB7LB6FB7LB7LB6FB6FB6FB8RB9XB8RB9XB:^B=qB=qB>wB?}B@�BA�BA�BB�BC�BC�BB�BF�BC�BA�BC�BI�BI�BI�BK�BM�BN�BN�BO�BQ�BS�BS�B[#B^5BbNBdZBe`BffBffBffBffBhsBe`Be`BffBgmBm�Bn�Bq�Br�Bu�Bv�Bw�By�B{�B}�B~�B� B�B�B�7B�=B�=B�DB�PB�bB�uB��B��B��B��B��B��B��B��B��B�B�B�3B�9B�qB�jB�jB�qB�qB��BƨBɺB��B��B��B��B��B��B��B�
B�B�B�B�5B�ZB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	%B	DB	bB	uB	�B	�B	�B	�B	�B	$�B	)�B	-B	.B	/B	/B	/B	0!B	2-B	2-B	33B	5?B	8RB	=qB	A�B	G�B	K�B	L�B	O�B	S�B	W
B	[#B	_;B	bNB	cTB	e`B	iyB	n�B	n�B	p�B	r�B	t�B	y�B	|�B	}�B	� B	�B	�B	�+B	�1B	�1B	�7B	�DB	�VB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�-B	�-B	�9B	�LB	�XB	�dB	�jB	�qB	�wB	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�ZB	�`B	�fB	��B	��B
	�B
.B
�B
 B
)�B
1[B
6�B
>B
DgB
L0B
QhB
V�B
ZB
_pB
d�B
i�B
n/B
r|B
s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�B�yB�yB�yB�B�yB�B��B�B��B�B/`B8�BUABa�Bj�Bi�Bm�Bo�Bs�ByB{%B�\B�bB��B��B��B��B��B��B��B��B��B�|B�oB�^B�RB|.Bv	Bh�Bd�B^~BY`BTABKB<�B2yB-[B#B�B�B�BtB�JB�B��B��B݁B��B�wB�GB�B��B}HBuBj�B[BQCBIB>�B+bB�B
�FB
��B
�kB
��B
�ZB
��B
v1B
\�B
M>B
EB
;�B
/�B
%QB
	B

�B	�ZB	ްB	�HB	��B	�PB	��B	��B	��B	��B	mB	c�B	MSB	CB	=�B	6�B	,�B	!NB	B	�B��B�VB��B�QB��B��B��B��B�FB�B��B��B��B��BtOBkBc�Be�BiBjBjBkBm%Bn+Bo1Bp7Bp7Bm%BlBhBgBgBd�Be�Bd�BiBp8BxhBynBv]BrDBq>Bp8Bp8Bn-Bn-Bn-BiBgBhBgBc�B_�BX�BT�BQ�BQ�BP~BQ�BT�BS�BOwBMlBMlBLfBHMBHMBJZBITBJZBFBBGHBJ[BD6B@B@BC0BNrBPBMmBFBBFBBHNBGHBGHBIUBE=BC1BB+B@B<B;B6�B6�B3�B2�B3�B2�B3�B2�B1�B2�B0�B0�B/�B.�B-�B/�B-�B.�B-�B.�B.�B-�B-�B-�B/�B0�B/�B0�B1�B4�B4�B5�B6�B7�B8�B8�B9�B;B;B9�B>B;B8�B;BA(BA(BA(BC5BEABFGBFGBGMBIZBKfBKfBR�BU�BY�B[�B\�B]�B]�B]�B]�B_�B\�B\�B]�B^�Bd�BfBiBjBm0Bn6Bo<BqHBsTBu`BvfBwlBz~B|�B��B��B��B��B��B��B��B��B�B�B�B�B�)B�0B�BB�TB�rB��B��B��B��B��B��B��B��B��B�B�#B�;B�GB�MB�MB�MB�`B�`B�rB�xB�xBхB՜B��B��B��B��B�B�B�B�B�"B�/B�;B�AB�MB�TB�ZB�`B�`B�`B�wB��B	�B	�B	
�B	�B	B	B	B	B	@B	!_B	$qB	%wB	&~B	&~B	&~B	'�B	)�B	)�B	*�B	,�B	/�B	4�B	8�B	?B	C(B	D.B	G@B	KXB	NjB	R�B	V�B	Y�B	Z�B	\�B	`�B	e�B	e�B	hB	jB	lB	q9B	tLB	uRB	w^B	xdB	{vB	~�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�9B	�KB	�WB	�]B	�]B	�]B	�]B	�]B	�cB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�,B	�8B	�>B	�>B	�EB	�KB	�QB	�]B	�cB	�iB	�vB	�{B	ӁB	ԇB	֓B	ךB	ךB	ؠB	ؠB	ؠB	ؠB	ڬB	۲B	ܸG�O�B	�$B	��B
EB
�B
�B
cB
!B
(�B
.B
5cB
;�B
C�B
H�B
M�B
QqB
V�B
\JB
a B
e�B
i�B
j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170856    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170856  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170856  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                