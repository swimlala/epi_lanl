CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:59Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190559  20181005190559  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)���l1   @��*�8��@0�?|�h�c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  @���AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���C  C  C�fC  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  D fD �fD  D� D  D� D  D� D  D�fDfD� D  D� D  D�fD  D� D��D	y�D
  D
� D  D� D  D� DfD�fD  D� D��D� D  D� D  D� D  D� D  D� D��D� D  Dy�D  D�fD  D� D  D� D  D�fD  D� D  D� D  D� D��Dy�D  D� D��D� D   D � D!  D!y�D"  D"� D#  D#� D#��D$� D%fD%� D%��D&y�D&��D'� D(  D(� D)  D)� D*  D*�fD+fD+�fD,fD,� D-  D-� D.  D.y�D.��D/� D0fD0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:fD:�fD;  D;� D<  D<� D=  D=�fD>fD>� D?  D?y�D?��D@� DAfDA� DB  DB�fDCfDC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT�fDU  DU� DVfDV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[�fD\  D\� D]  D]�fD^  D^� D^��D_� D`  D`�fDafDa�fDbfDb� Dc  Dc� Dd  Dd� De  De� DffDf�fDg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDpfDp� Dp��Dq� Dr  Dr�fDs  Ds� Dt  Dty�Dt��Du� Dv  Dv� Dv��Dw� Dw�3Dy��D�*=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
AQ�A$Q�ADQ�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��B��qB��qB��qB��qB��B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB�#�BЊ>BԊ>BؽqBܽqB�qB�qB�qB�qB�qB��qB��B��qC EC^�C^�CEC^�C
^�C^�C^�C^�C^�C^�CxRC^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�"�C�"�C�/\C�"�C�/\C�<)C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�"�C�"�C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�<)C�<)C�<)C�/\C�/\C�/\C�/\C�"�C�"�C�/\C�/\C�/\C�/\C�/\C�<)C�<)C�/\C�/\C�"�C�/\C�/\C�"�C�/\C�/\C�/\C�/\D D �D�D��D�D��D�D��D�D�DD��D�D��D�D�D�D��D	HD	�HD
�D
��D�D��D�D��DD�D�D��DHD��D�D��D�D��D�D��D�D��DHD��D�D�HD�D�D�D��D�D��D�D�D�D��D�D��D�D��DHD�HD�D��DHD��D �D ��D!�D!�HD"�D"��D#�D#��D$HD$��D%D%��D&HD&�HD'HD'��D(�D(��D)�D)��D*�D*�D+D+�D,D,��D-�D-��D.�D.�HD/HD/��D0D0��D1�D1��D2�D2��D3�D3�HD4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9�D:D:�D;�D;��D<�D<��D=�D=�D>D>��D?�D?�HD@HD@��DADA��DB�DB�DCDC��DD�DD��DE�DE��DFDF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�DS�DS��DT�DT�DU�DU��DVDV��DW�DW��DXHDX��DY�DY��DZ�DZ��D[�D[�D\�D\��D]�D]�D^�D^��D_HD_��D`�D`�DaDa�DbDb��Dc�Dc��Dd�Dd��De�De��DfDf�Dg�Dg��DhDh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�DpDp��DqHDq��Dr�Dr�Ds�Ds��Dt�Dt�HDuHDu��Dv�Dv��DwHDw��Dw��Dy��D�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�VA�Q�A�O�A�Q�A�XA�\)A�\)A�^5A�jA�n�A�p�A�p�A�r�A�r�A�r�A�t�A�v�A�v�A�x�A�x�A�x�A�z�A�t�A�`BA�Q�A�M�A�O�A�Q�A�XA�XA�33A���A��A��A���A��A��A��A���A���A�oA��A�&�A�-A� �A�AƍPA�VA��A�{A���A�7LA���A���A��!A���A��A���A�{A�VA�I�A���A�1A��A�Q�A�ĜA�33A��9A��\A��wA�ĜA�A��/A��
A��A�VA��#A��!A�A�~�A�%A���A�x�A���A�K�A��A�A��^A��PA�(�A�33A�n�A�?}A��\A��^A���A�r�A�hsA�^5A�A�-A�v�A��A��A��Az�RAv��ArJAo33AmƨAj�Ah��Af�Ad��Aa�FA^�jA\$�AZ�!AY��AY/AWdZAS��AQ�APZAN��AIK�AFM�AD�uAC��AB��ABbA@�A?33A<5?A9��A7XA6 �A5O�A4��A2bNA0r�A.v�A-"�A,�\A+t�A*=qA)"�A'�A%��A#�wA#�A"��A"~�A!\)A n�A��Az�A`BA1'A��AoA �A&�A �A&�A=qA�AbAS�A��AA��A  A"�A�A�mA��A
ȴA	��A	AM�A��A��A/A�\A^5A=qAA�RAA^5A��AƨA��A �y@��@�
=@���@���@��;@�Z@�1'@�7L@��@�r�@�+@��@��D@� �@�;d@��@���@�V@�F@�@�v�@��@�j@��;@�F@�5?@�@��@��@ް!@۶F@ە�@�S�@�v�@�A�@�+@�5?@�@ա�@��/@�
=@ҧ�@�n�@�-@ѩ�@�?}@��@Ѓ@ϥ�@�K�@�n�@�`B@�Ĝ@���@�{@ȃ@ȋD@ȃ@��@Ƈ+@�M�@�@�7L@��@�Q�@�|�@��y@�ff@�7L@���@�l�@��y@���@�ff@�-@�x�@�z�@��@��;@�\)@��@��+@�M�@��T@��#@�@���@��#@�p�@�p�@�J@���@�Ĝ@�r�@�Z@��@�dZ@�33@�@��!@��y@���@�$�@�/@��9@��@�Z@�(�@�t�@���@�@��h@�G�@�(�@�;d@�+@���@���@�V@�$�@�J@�$�@�5?@���@�O�@��/@��@���@�t�@�\)@�C�@�ff@���@��h@�%@���@��D@�r�@��;@�|�@���@���@��+@�^5@�@���@���@�`B@�%@��/@�&�@�G�@�O�@��@�hs@�V@��`@��/@���@���@���@�Ĝ@��D@�I�@�b@��m@���@�dZ@�"�@��H@���@��+@�V@��#@�hs@�hs@�`B@�?}@��@���@��@��
@��;@��;@���@�dZ@��@���@�n�@�ff@�n�@�O�@��j@�j@�1'@� �@��
@�ƨ@��w@��@�@��!@�v�@�^5@�$�@��T@���@���@��-@�x�@�&�@�7L@�&�@�&�@��@���@���@�j@�1@��
@���@�S�@�o@��H@��R@�ff@���@�?}@�Ĝ@�I�@��@��;@�l�@�ȴ@�ȴ@�ȴ@��R@���@�=q@��@�@���@�x�@�hs@�`B@��7@�X@��/@��@�9X@��w@��;@��F@���@�dZ@��@�ȴ@���@�v�@�5?@��^@�X@��@���@�z�@� �@��
@�S�@�+@�C�@��@�
=@�o@���@��\@�n�@�=q@��@���@��@�p�@�O�@���@��D@�1'@���@�t�@�33@���@�n�@�M�@�-@��T@��@�X@�7L@�/@��@��@��U@z��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�K�A�VA�Q�A�O�A�Q�A�XA�\)A�\)A�^5A�jA�n�A�p�A�p�A�r�A�r�A�r�A�t�A�v�A�v�A�x�A�x�A�x�A�z�A�t�A�`BA�Q�A�M�A�O�A�Q�A�XA�XA�33A���A��A��A���A��A��A��A���A���A�oA��A�&�A�-A� �A�AƍPA�VA��A�{A���A�7LA���A���A��!A���A��A���A�{A�VA�I�A���A�1A��A�Q�A�ĜA�33A��9A��\A��wA�ĜA�A��/A��
A��A�VA��#A��!A�A�~�A�%A���A�x�A���A�K�A��A�A��^A��PA�(�A�33A�n�A�?}A��\A��^A���A�r�A�hsA�^5A�A�-A�v�A��A��A��Az�RAv��ArJAo33AmƨAj�Ah��Af�Ad��Aa�FA^�jA\$�AZ�!AY��AY/AWdZAS��AQ�APZAN��AIK�AFM�AD�uAC��AB��ABbA@�A?33A<5?A9��A7XA6 �A5O�A4��A2bNA0r�A.v�A-"�A,�\A+t�A*=qA)"�A'�A%��A#�wA#�A"��A"~�A!\)A n�A��Az�A`BA1'A��AoA �A&�A �A&�A=qA�AbAS�A��AA��A  A"�A�A�mA��A
ȴA	��A	AM�A��A��A/A�\A^5A=qAA�RAA^5A��AƨA��A �y@��@�
=@���@���@��;@�Z@�1'@�7L@��@�r�@�+@��@��D@� �@�;d@��@���@�V@�F@�@�v�@��@�j@��;@�F@�5?@�@��@��@ް!@۶F@ە�@�S�@�v�@�A�@�+@�5?@�@ա�@��/@�
=@ҧ�@�n�@�-@ѩ�@�?}@��@Ѓ@ϥ�@�K�@�n�@�`B@�Ĝ@���@�{@ȃ@ȋD@ȃ@��@Ƈ+@�M�@�@�7L@��@�Q�@�|�@��y@�ff@�7L@���@�l�@��y@���@�ff@�-@�x�@�z�@��@��;@�\)@��@��+@�M�@��T@��#@�@���@��#@�p�@�p�@�J@���@�Ĝ@�r�@�Z@��@�dZ@�33@�@��!@��y@���@�$�@�/@��9@��@�Z@�(�@�t�@���@�@��h@�G�@�(�@�;d@�+@���@���@�V@�$�@�J@�$�@�5?@���@�O�@��/@��@���@�t�@�\)@�C�@�ff@���@��h@�%@���@��D@�r�@��;@�|�@���@���@��+@�^5@�@���@���@�`B@�%@��/@�&�@�G�@�O�@��@�hs@�V@��`@��/@���@���@���@�Ĝ@��D@�I�@�b@��m@���@�dZ@�"�@��H@���@��+@�V@��#@�hs@�hs@�`B@�?}@��@���@��@��
@��;@��;@���@�dZ@��@���@�n�@�ff@�n�@�O�@��j@�j@�1'@� �@��
@�ƨ@��w@��@�@��!@�v�@�^5@�$�@��T@���@���@��-@�x�@�&�@�7L@�&�@�&�@��@���@���@�j@�1@��
@���@�S�@�o@��H@��R@�ff@���@�?}@�Ĝ@�I�@��@��;@�l�@�ȴ@�ȴ@�ȴ@��R@���@�=q@��@�@���@�x�@�hs@�`B@��7@�X@��/@��@�9X@��w@��;@��F@���@�dZ@��@�ȴ@���@�v�@�5?@��^@�X@��@���@�z�@� �@��
@�S�@�+@�C�@��@�
=@�o@���@��\@�n�@�=q@��@���@��@�p�@�O�@���@��D@�1'@���@�t�@�33@���@�n�@�M�@�-@��T@��@�X@�7L@�/@��@��@��U@z��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B.B@�B^5B�+B�B}�B��B�B�B��B	+B	oB	(�B	5?B	<jB	I�B	Q�B	T�B	bNB	ffB	o�B	�hB	B	�fB
�B
F�B
�XBD�BjB� B�B�PB��B�XB�qB�}BɺB��B�mB�B�B��B��B�B$�B"�B�B\B	7B	7BJBPB��B��B�B�5B��B��BĜB�-B��B�uB�Bs�BR�B33B{B  B
�B
�`B
�NB
��B
�RB
��B
�7B
o�B
`BB
YB
K�B
.B
�B
DB	��B	�
B	�^B	��B	�=B	z�B	dZB	T�B	C�B	49B	+B	�B	�B	\B	
=B	%B��B�B�HB�
B��B�B��B��B��B��B��B�uB�hB�\B�VB�bB�hB�hB�bB�uB��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�RB�qBƨBƨBÖB��B�}B�qB�wB�^B�LB�FB�XB�wB��B��BBB��B��B��B�}B�qB�jB�qB�dB�^B�XB�XB�RB�RB�^B�dB�dB�^B�FB�'B�B�3B�XB�qBĜBB��B��B��B��B�B��B��BĜBÖBŢBǮBǮBǮB��B��B��B��B��B��B��B��B�
B�B�B�/B�;B�BB�BB�ZB�B�B�B�B��B�B��B��B��B��B	  B	B		7B	\B	�B	�B	�B	!�B	!�B	"�B	&�B	'�B	(�B	'�B	-B	5?B	:^B	:^B	;dB	D�B	E�B	G�B	J�B	J�B	R�B	T�B	XB	XB	YB	XB	XB	YB	ZB	\)B	\)B	^5B	aHB	bNB	cTB	gmB	iyB	k�B	l�B	l�B	m�B	o�B	o�B	q�B	t�B	w�B	}�B	�B	�B	�7B	�DB	�JB	�VB	�\B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�RB	�RB	�XB	�RB	�XB	�qB	�}B	B	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�/B	�;B	�`B	�mB	�fB	�fB	�fB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
B
B
+B
1B
1B
	7B

=B
	7B

=B
DB
JB
JB
JB
PB
\B
bB
hB
hB
hB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
'�B
%�B
5�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B.B@�B^5B�+B�B}�B��B�B�B��B	+B	oB	(�B	5?B	<jB	I�B	Q�B	T�B	bNB	ffB	o�B	�hB	B	�fB
�B
F�B
�XBD�BjB� B�B�PB��B�XB�qB�}BɺB��B�mB�B�B��B��B�B$�B"�B�B\B	7B	7BJBPB��B��B�B�5B��B��BĜB�-B��B�uB�Bs�BR�B33B{B  B
�B
�`B
�NB
��B
�RB
��B
�7B
o�B
`BB
YB
K�B
.B
�B
DB	��B	�
B	�^B	��B	�=B	z�B	dZB	T�B	C�B	49B	+B	�B	�B	\B	
=B	%B��B�B�HB�
B��B�B��B��B��B��B��B�uB�hB�\B�VB�bB�hB�hB�bB�uB��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�RB�qBƨBƨBÖB��B�}B�qB�wB�^B�LB�FB�XB�wB��B��BBB��B��B��B�}B�qB�jB�qB�dB�^B�XB�XB�RB�RB�^B�dB�dB�^B�FB�'B�B�3B�XB�qBĜBB��B��B��B��B�B��B��BĜBÖBŢBǮBǮBǮB��B��B��B��B��B��B��B��B�
B�B�B�/B�;B�BB�BB�ZB�B�B�B�B��B�B��B��B��B��B	  B	B		7B	\B	�B	�B	�B	!�B	!�B	"�B	&�B	'�B	(�B	'�B	-B	5?B	:^B	:^B	;dB	D�B	E�B	G�B	J�B	J�B	R�B	T�B	XB	XB	YB	XB	XB	YB	ZB	\)B	\)B	^5B	aHB	bNB	cTB	gmB	iyB	k�B	l�B	l�B	m�B	o�B	o�B	q�B	t�B	w�B	}�B	�B	�B	�7B	�DB	�JB	�VB	�\B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�RB	�RB	�XB	�RB	�XB	�qB	�}B	B	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�/B	�;B	�`B	�mB	�fB	�fB	�fB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
B
B
+B
1B
1B
	7B

=B
	7B

=B
DB
JB
JB
JB
PB
\B
bB
hB
hB
hB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
'�B
%�B
5�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.37 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190559                              AO  ARCAADJP                                                                    20181005190559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190559  QCF$                G�O�G�O�G�O�8000            