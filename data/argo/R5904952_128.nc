CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:33Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190533  20181005190533  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��j1   @��jSo��@0��"��`�c����+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�33A�33A�33A�  A���A���A�  B   B  B��B  B   B(  B0  B8  B?��BH  BP  BX  B`  BhffBpffBx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC}�fC�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C��C�  C�  C��3C��3C��3C��3C��3C��3C�  C��3C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D��Dy�D��D� DfD� D��D� DfD� D	  D	y�D	��D
y�D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D�fDfD�fD  D� D  D� DfD� D  D� D  Dy�D��D y�D!  D!�fD"fD"�fD#  D#� D$  D$� D$��D%� D&  D&� D'  D'y�D(  D(�fD)  D)�fD*fD*� D+  D+� D+��D,� D-fD-� D.  D.y�D/  D/� D0  D0� D0��D1y�D2  D2y�D2��D3y�D4  D4�fD5  D5y�D5��D6� D7fD7� D8  D8�fD9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR�fDS  DSy�DT  DT� DU  DU� DU��DV� DWfDW� DW��DX� DY  DYy�DZ  DZ� D[fD[�fD\fD\�fD]  D]� D^  D^� D_fD_� D`  D`� Da  Da�fDb  Dby�DcfDc� Dd  Dd�fDe  De� De��Dfy�Df��Dg� DhfDh�fDifDi� Dj  Dj� Dj��Dk� Dl  Dl�fDmfDm�fDnfDn�fDo  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Duy�DvfDv�fDw  Dw� Dw��Dy��D�6D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�
=A�A#�AC�Ac�A�A���A���A���A�Aҏ\A�\A�B �HB�HBz�B�HB �HB(�HB0�HB8�HB@z�BH�HBP�HBX�HB`�HBiG�BqG�Bx�HB�p�B�p�B���B���B�p�B�p�B�p�B�p�B�p�B�p�B���B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�Bأ�Bܣ�B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�C 8RCQ�C8RC8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL�CN8RCP8RCRQ�CT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd�Cf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|�C~�C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�\C�\C�\C�)C�)C�)C�)C�(�C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�(�C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�(�C�)C�)C�)C�\C�\C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�\C�)C�(�C�)C�)C�)C�(�C�(�C�)C�)C�\C�\C�\C�\C�\C�\C�)C�\C�)C�)C�)C�(�C�)C�)C�)C�)D D �DD�DD�DD��D�D��D�D�DzD�D�D�DzD�D	D	��D
�D
��DD�DD�DD�DD�DD�DD�zDD�DD�DD�DD�DD�D�D��DD�DD�DD�zDzD�zDD�DD�DzD�DD�DD��D �D ��D!D!�zD"zD"�zD#D#�D$D$�D%�D%�D&D&�D'D'��D(D(�zD)D)�zD*zD*�D+D+�D,�D,�D-zD-�D.D.��D/D/�D0D0�D1�D1��D2D2��D3�D3��D4D4�zD5D5��D6�D6�D7zD7�D8D8�zD9zD9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?��D@D@�DADA�DBDB��DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRzDR�zDSDS��DTDT�DUDU�DV�DV�DWzDW�DX�DX�DYDY��DZDZ�D[zD[�zD\zD\�zD]D]�D^D^�D_zD_�D`D`�DaDa�zDbDb��DczDc�DdDd�zDeDe�Df�Df��Dg�Dg�DhzDh�zDizDi�DjDj�Dk�Dk�DlDl�zDmzDm�zDnzDn�zDoDo��DpDp�DqDq�DrDr�DsDs�zDtDt�DuDu��DvzDv�zDwDw�Dw��Dy��D�=D�Ƹ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A���A���A�  A�A�%A�
=A�{A�VA��A�"�A��A� �A�"�A�&�A�&�A�+A�+A�+A�-A�33A�5?A�9XA�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�K�A�K�A�K�A�M�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�K�A�33A���A�G�A��mA�ƨA̸RA�-A�"�A�^5AȺ^A�dZAƍPA�Aţ�A�ffA�t�A� �A�ȴA���A��FA�|�A�bA���A��jA�9XA�I�A��A��!A�7LA���A�^5A��9A�ffA�hsA�/A�r�A���A�~�A��7A��A�%A��A�$�A�VA�+A���A��A�(�A���A�oA�7LA���A���A���A���A�A��A���A�Q�A���A�7LA�#Az �AvI�Aq�An�RAj�HAf��Aa�A\ �AZz�AZ(�AX��AUXATJAR��APffAOp�ANȴAM&�AJ��AHE�AF�+AE�;AC��AC+AA�hA>�+A<(�A:I�A9�7A8��A8��A7��A3�A1XA0�\A/�A.1A-G�A-/A-&�A-%A,E�A)�;A(ffA't�A& �A%O�A#��A"�!A"bNA!�mA!��A!/A (�A  AjAI�AbNAffA�mAx�A�A��A-A$�An�A�yAȴA�HA
=At�A�A�DA �A��AbNA��A
�A	��A	+A~�A^5A5?A��A�A;dA��A{AoAZAXAr�A�A%A �\@���@�J@��;@��^@�9X@�ȴ@�{@�`B@�1@��@�G�@�F@�;d@�G�@���@�`B@��/@�@�Q�@���@�dZ@�R@�h@�+@�ff@�-@��@��@���@�@�?}@�z�@��@�M�@ٙ�@�Ĝ@�S�@�V@�O�@��`@ԓu@�I�@�r�@�Ĝ@ԃ@�bN@�Z@�A�@ҏ\@��@Ѻ^@�%@�&�@Ѳ-@�G�@�5?@�7L@̃@�1'@�  @�  @�A�@���@�C�@�=q@�/@ȼj@�(�@��
@�C�@��@Ə\@��@�x�@�hs@�`B@�O�@�%@���@�A�@�hs@�Z@�Q�@�|�@���@��@�"�@��9@��P@���@�bN@���@��
@�@�O�@��@�t�@�K�@���@�V@��@���@�I�@��@�=q@��!@�ȴ@�n�@���@�bN@�b@���@���@�1@�(�@���@�;d@�5?@�=q@�@���@��h@���@�?}@�%@��j@��;@���@��F@�;d@�\)@�C�@���@�J@���@�p�@���@�j@�(�@��@��!@��+@���@��D@�1@�bN@�I�@�  @��
@��P@���@���@��R@�V@��@���@���@�hs@�X@�7L@���@���@�(�@�(�@��D@���@��+@��@��@�Ĝ@�r�@��@�+@�ȴ@�ff@���@��h@�/@��/@�1'@��
@��@��w@�j@� �@�v�@���@��-@��-@�@��^@��-@��@�X@�&�@�z�@�A�@�1@��;@���@�K�@�+@��@�^5@�-@�^5@���@���@�~�@�V@��@�@���@��@��7@���@�A�@�ƨ@���@���@�33@��y@��y@��H@��@��\@��@�@��@�`B@�V@���@��/@�j@��@��@�o@��H@�ȴ@�v�@�$�@�$�@���@���@��\@�ff@��T@�G�@�A�@���@���@��@�1@�1'@��@���@� �@�l�@�o@���@�33@��P@��!@��\@�~�@�v�@�^5@���@���@��^@���@���@���@��D@�bN@�b@�  @��;@�l�@��y@��H@��@���@���@���@��+@��@�o@�s�@mG�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A���A���A�  A�A�%A�
=A�{A�VA��A�"�A��A� �A�"�A�&�A�&�A�+A�+A�+A�-A�33A�5?A�9XA�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�K�A�K�A�K�A�M�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�K�A�33A���A�G�A��mA�ƨA̸RA�-A�"�A�^5AȺ^A�dZAƍPA�Aţ�A�ffA�t�A� �A�ȴA���A��FA�|�A�bA���A��jA�9XA�I�A��A��!A�7LA���A�^5A��9A�ffA�hsA�/A�r�A���A�~�A��7A��A�%A��A�$�A�VA�+A���A��A�(�A���A�oA�7LA���A���A���A���A�A��A���A�Q�A���A�7LA�#Az �AvI�Aq�An�RAj�HAf��Aa�A\ �AZz�AZ(�AX��AUXATJAR��APffAOp�ANȴAM&�AJ��AHE�AF�+AE�;AC��AC+AA�hA>�+A<(�A:I�A9�7A8��A8��A7��A3�A1XA0�\A/�A.1A-G�A-/A-&�A-%A,E�A)�;A(ffA't�A& �A%O�A#��A"�!A"bNA!�mA!��A!/A (�A  AjAI�AbNAffA�mAx�A�A��A-A$�An�A�yAȴA�HA
=At�A�A�DA �A��AbNA��A
�A	��A	+A~�A^5A5?A��A�A;dA��A{AoAZAXAr�A�A%A �\@���@�J@��;@��^@�9X@�ȴ@�{@�`B@�1@��@�G�@�F@�;d@�G�@���@�`B@��/@�@�Q�@���@�dZ@�R@�h@�+@�ff@�-@��@��@���@�@�?}@�z�@��@�M�@ٙ�@�Ĝ@�S�@�V@�O�@��`@ԓu@�I�@�r�@�Ĝ@ԃ@�bN@�Z@�A�@ҏ\@��@Ѻ^@�%@�&�@Ѳ-@�G�@�5?@�7L@̃@�1'@�  @�  @�A�@���@�C�@�=q@�/@ȼj@�(�@��
@�C�@��@Ə\@��@�x�@�hs@�`B@�O�@�%@���@�A�@�hs@�Z@�Q�@�|�@���@��@�"�@��9@��P@���@�bN@���@��
@�@�O�@��@�t�@�K�@���@�V@��@���@�I�@��@�=q@��!@�ȴ@�n�@���@�bN@�b@���@���@�1@�(�@���@�;d@�5?@�=q@�@���@��h@���@�?}@�%@��j@��;@���@��F@�;d@�\)@�C�@���@�J@���@�p�@���@�j@�(�@��@��!@��+@���@��D@�1@�bN@�I�@�  @��
@��P@���@���@��R@�V@��@���@���@�hs@�X@�7L@���@���@�(�@�(�@��D@���@��+@��@��@�Ĝ@�r�@��@�+@�ȴ@�ff@���@��h@�/@��/@�1'@��
@��@��w@�j@� �@�v�@���@��-@��-@�@��^@��-@��@�X@�&�@�z�@�A�@�1@��;@���@�K�@�+@��@�^5@�-@�^5@���@���@�~�@�V@��@�@���@��@��7@���@�A�@�ƨ@���@���@�33@��y@��y@��H@��@��\@��@�@��@�`B@�V@���@��/@�j@��@��@�o@��H@�ȴ@�v�@�$�@�$�@���@���@��\@�ff@��T@�G�@�A�@���@���@��@�1@�1'@��@���@� �@�l�@�o@���@�33@��P@��!@��\@�~�@�v�@�^5@���@���@��^@���@���@���@��D@�bN@�b@�  @��;@�l�@��y@��H@��@���@���@���@��+@��@�o@�s�@mG�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuBuBuB{B{B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B)�B)�B(�B(�B(�B(�B(�B(�B(�B'�B'�B&�B#�B%�BbNB�hB��B�B�XB��B�fB�B�B��BVB�B�B�B�B%�B-BD�BL�BQ�B[#BK�BF�BE�BE�BE�BD�BB�B=qB;dB8RB+B%�B�BbBB�sB��B��BÖB�?B�{BiyBYBN�BC�B0!B
��B
�B
��B
�dB
�DB
u�B
gmB
N�B
+B	��B	�9B	�=B	n�B	L�B	H�B	2-B	bB�B��BÖB�}B�RB�B��B��B��B��B��B��B�7B�+B�B�B}�Bz�Bz�B|�B~�B� B� B�B� B�B�bB��B��B��B�B�B�B�B�B�B�?B�qBÖBÖBƨB��BɺBǮBƨBĜBĜB��B�?B�LB��BĜBƨBƨBȴBÖB��B�}B��BǮB��B��B�)B��BĜBŢBĜBÖB�dB�9B�3B�'B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�9B�LB�RB�XB�^B�^B�}B��BÖBƨBƨBȴB��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�5B�BB�;B�;B�;B�BB�TB�`B�fB�mB�yB�yB�B�B�B��B��B��B	B	B��B��B	B	B	+B	oB	{B	JB	
=B	
=B	DB	bB	�B	�B	'�B	,B	1'B	8RB	8RB	6FB	49B	=qB	F�B	I�B	J�B	K�B	M�B	N�B	O�B	N�B	M�B	L�B	G�B	E�B	F�B	D�B	F�B	G�B	I�B	ZB	`BB	e`B	n�B	p�B	ffB	_;B	]/B	YB	[#B	\)B	_;B	cTB	o�B	� B	}�B	�1B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�LB	�RB	�XB	�RB	�FB	�dB	�qB	�wB	��B	��B	��B	��B	��B	ĜB	ÖB	B	ŢB	ĜB	B	��B	�jB	�XB	�^B	�qB	��B	��B	��B	��B	�}B	�}B	�}B	B	ÖB	B	B	ÖB	ĜB	ƨB	ƨB	ƨB	ŢB	ǮB	��B	ɺB	ǮB	ƨB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�)B	�BB	�BB	�/B	�/B	�/B	�5B	�BB	�HB	�HB	�HB	�ZB	�ZB	�fB	�fB	�mB	�fB	�fB	�`B	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
  B
B

=B
1B
+B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
DB
	B
�B
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BuBuBuB{B{B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B)�B)�B(�B(�B(�B(�B(�B(�B(�B'�B'�B&�B#�B%�BbNB�hB��B�B�XB��B�fB�B�B��BVB�B�B�B�B%�B-BD�BL�BQ�B[#BK�BF�BE�BE�BE�BD�BB�B=qB;dB8RB+B%�B�BbBB�sB��B��BÖB�?B�{BiyBYBN�BC�B0!B
��B
�B
��B
�dB
�DB
u�B
gmB
N�B
+B	��B	�9B	�=B	n�B	L�B	H�B	2-B	bB�B��BÖB�}B�RB�B��B��B��B��B��B��B�7B�+B�B�B}�Bz�Bz�B|�B~�B� B� B�B� B�B�bB��B��B��B�B�B�B�B�B�B�?B�qBÖBÖBƨB��BɺBǮBƨBĜBĜB��B�?B�LB��BĜBƨBƨBȴBÖB��B�}B��BǮB��B��B�)B��BĜBŢBĜBÖB�dB�9B�3B�'B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�9B�LB�RB�XB�^B�^B�}B��BÖBƨBƨBȴB��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�5B�BB�;B�;B�;B�BB�TB�`B�fB�mB�yB�yB�B�B�B��B��B��B	B	B��B��B	B	B	+B	oB	{B	JB	
=B	
=B	DB	bB	�B	�B	'�B	,B	1'B	8RB	8RB	6FB	49B	=qB	F�B	I�B	J�B	K�B	M�B	N�B	O�B	N�B	M�B	L�B	G�B	E�B	F�B	D�B	F�B	G�B	I�B	ZB	`BB	e`B	n�B	p�B	ffB	_;B	]/B	YB	[#B	\)B	_;B	cTB	o�B	� B	}�B	�1B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�LB	�RB	�XB	�RB	�FB	�dB	�qB	�wB	��B	��B	��B	��B	��B	ĜB	ÖB	B	ŢB	ĜB	B	��B	�jB	�XB	�^B	�qB	��B	��B	��B	��B	�}B	�}B	�}B	B	ÖB	B	B	ÖB	ĜB	ƨB	ƨB	ƨB	ŢB	ǮB	��B	ɺB	ǮB	ƨB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�)B	�BB	�BB	�/B	�/B	�/B	�5B	�BB	�HB	�HB	�HB	�ZB	�ZB	�fB	�fB	�mB	�fB	�fB	�`B	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
  B
B

=B
1B
+B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
DB
	B
�B
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190533                              AO  ARCAADJP                                                                    20181005190533    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190533  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190533  QCF$                G�O�G�O�G�O�8000            