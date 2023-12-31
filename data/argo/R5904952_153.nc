CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:39Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190539  20181005190539  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�ߩ���1   @�ߪ5��@0�O�;dZ�c�$�/1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A��A   A@  A`  A�  A�33A�  A�  A�  A�33A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�33B�  B�  B�33B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C�C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D�fD  Dy�D��Dy�D  D� D  D� D��Dy�D��Dy�D��Dy�D  D� D  D� D  D� D  D� D��Dy�D��Dy�D��Dy�D��D� DfD� D��D� D  D� D   D � D!  D!� D"fD"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,�fD-fD-�fD.fD.�fD/fD/�fD0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D=  D=�fD>fD>� D?  D?y�D?��D@� DA  DA�fDBfDB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DNfDN�fDOfDO�fDP  DP� DQ  DQ� DQ��DRy�DR��DSy�DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[y�D\  D\�fD]fD]� D]��D^y�D_  D_�fD`fD`�fDafDa�fDb  Dby�Db��Dc� Dd  Dd� De  De� Df  Dfy�Df��Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr�fDsfDs� Ds��Dt� Du  Du�fDv  Dv� Dv��Dws3DyhRD�C�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@��HA
>A%p�AEp�Aep�A��RA��A��RA��RA¸RA��A�RA�RB\)B	\)BB\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)BiBq\)Bx��B��B��GB��B��B��GB��GB��B��B��B��GB��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
C=pCW
CW
Cp�C
W
CW
Cp�Cp�CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CHp�CJp�CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
Ctp�Cvp�CxW
CzW
C|W
C~W
C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C��C��C�+�C��C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C��C��C�+�C�8RC�+�C�+�C�+�C�+�C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C��C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�+�C��C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D)D��D�D��D�D��D�D��D]D�]D�D��D�D��D�D��D	�D	��D
�D
��D�D��D]D��D�D�)D�D�]D]D�]D�D��D�D��D]D�]D]D�]D]D�]D�D��D�D��D�D��D�D��D]D�]D]D�]D]D�]D]D��D)D��D]D��D�D��D �D ��D!�D!��D")D"�)D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+�]D,�D,�)D-)D-�)D.)D.�)D/)D/�)D0�D0��D1�D1��D2�D2�]D3�D3��D4�D4��D5�D5��D6�D6��D7]D7��D8�D8��D9�D9��D:�D:��D;�D;��D<]D<�]D=�D=�)D>)D>��D?�D?�]D@]D@��DA�DA�)DB)DB�)DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK�)DL�DL��DM�DM��DN)DN�)DO)DO�)DP�DP��DQ�DQ��DR]DR�]DS]DS�]DT�DT��DU�DU�]DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[]D[�]D\�D\�)D])D]��D^]D^�]D_�D_�)D`)D`�)Da)Da�)Db�Db�]Dc]Dc��Dd�Dd��De�De��Df�Df�]Dg]Dg��Dh�Dh��Di�Di�)Dj�Dj��Dk�Dk��Dl]Dl�]Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�]Dq�Dq��Dr�Dr�)Ds)Ds��Dt]Dt��Du�Du�)Dv�Dv��Dw]Dw��Dy~D�NfD��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�1'A�9XA�7LA�7LA�9XA�7LA�9XA�;dA�;dA�7LA��A΋DA�`BA�I�A͙�A�Q�A�{A��A���A̲-A̓uA̅A�p�A�z�Ȁ\Ȁ\Ả7A�|�A�r�A�ZA�Q�A�I�A�&�A�?}A�O�A�l�A̋DA̕�A̕�A̕�A̙�A̧�A̡�Ḁ�A̶FA̸RA���A̸RA̰!A̗�A�t�A��mA�C�A�+A�p�A�{A�A�A�n�A�C�A���A�M�A��!A��!A���A�p�A��mA�A�\)A�ȴA��jA�x�A���A���A��yA�9XA��A��-A���A��HA�|�A��\A��\A��A�5?A���A�E�A���A�`BA��A�M�A��A�`BA��HA�dZA�;dA��RA�JA�M�A�A�I�A�?}A��A�1A���A�;dA�1A�jA�  A�z�A�n�A���A�A��RA|ȴAz�Aul�Ao`BAl�Aj1AhQ�Af�yAdI�A`��A_dZA^^5A\�yA[��AZ�AZ �AX��AV��AT  ARbNAQ�AK��AI��AF��AE;dAC�7AA�7A?��A<�jA8��A7��A7A5�7A4�A41A3�-A3oA17LA,��A+C�A*�!A*M�A)�A(��A(A%�FA$ �A"E�A �A�hA�AI�A��A�PAI�A��AVA��AJA�HA1AoA��A5?A��A��A�A%A��A�DA1A\)A �A
��A	A(�A�TA��A�!AA�A��AXAC�A+A�AM�A�;A33A�A�!AbNA�
A�A/AK�A Z@���@�5?@��j@� �@��\@���@���@�S�@�S�@��H@�-@��@���@�ȴ@�%@��@띲@�^5@�-@�  @�;d@�v�@�/@䛦@�  @�"�@���@�@� �@�|�@ޗ�@�x�@��@��@؋D@�5?@�O�@�I�@�\)@�@���@�M�@�p�@�z�@��@ΰ!@ͩ�@���@�r�@ˍP@�+@��@��@�ȴ@ʸR@���@�|�@�+@ʰ!@�{@�bN@Ƈ+@ŉ7@�%@�Ĝ@Ĭ@��@��@¸R@¸R@¸R@�v�@��@�p�@�7L@�V@��`@���@���@��P@��@���@�l�@�@��!@�^5@��^@��@��9@�Z@�  @��@��@��;@�C�@��y@�n�@�7L@���@���@��`@���@�z�@� �@�
=@��\@�M�@��#@��^@��h@��@��`@�b@���@�t�@�\)@�33@��H@�-@��T@��@�G�@�%@�j@�I�@� �@��F@�33@�ȴ@���@��+@��+@�@�@�X@�%@��9@�r�@�1@��P@�K�@�33@�@���@�v�@�J@��#@��-@�7L@���@�Ĝ@�Ĝ@�Ĝ@���@�1'@�ƨ@�l�@�C�@�+@��@�ȴ@�~�@�v�@�ff@�=q@���@��@�`B@�V@��/@���@�r�@�9X@�1@��@�|�@�;d@���@���@�E�@�J@���@��h@��@��`@��@���@�|�@�C�@���@���@���@��+@���@��#@��^@���@�Ĝ@��u@�bN@��F@�S�@�o@��@��!@��+@�ff@�-@���@�p�@�7L@��/@��D@�1'@��m@��P@�|�@��H@���@�V@��T@��^@��^@��h@�x�@�`B@�&�@��@��`@�Ĝ@��@�Z@�(�@���@���@��@�t�@�dZ@�K�@�
=@��H@���@���@�v�@�@�@���@�O�@��/@��u@�I�@���@��@�C�@��@��!@�E�@�$�@��@�{@�J@���@��#@���@�p�@�7L@���@��@���@���@��/@��9@�z�@�Q�@�I�@�(�@��@��@��F@���@�t�@�S�@�;d@�+@��6@z@h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$�A�1'A�9XA�7LA�7LA�9XA�7LA�9XA�;dA�;dA�7LA��A΋DA�`BA�I�A͙�A�Q�A�{A��A���A̲-A̓uA̅A�p�A�z�Ȁ\Ȁ\Ả7A�|�A�r�A�ZA�Q�A�I�A�&�A�?}A�O�A�l�A̋DA̕�A̕�A̕�A̙�A̧�A̡�Ḁ�A̶FA̸RA���A̸RA̰!A̗�A�t�A��mA�C�A�+A�p�A�{A�A�A�n�A�C�A���A�M�A��!A��!A���A�p�A��mA�A�\)A�ȴA��jA�x�A���A���A��yA�9XA��A��-A���A��HA�|�A��\A��\A��A�5?A���A�E�A���A�`BA��A�M�A��A�`BA��HA�dZA�;dA��RA�JA�M�A�A�I�A�?}A��A�1A���A�;dA�1A�jA�  A�z�A�n�A���A�A��RA|ȴAz�Aul�Ao`BAl�Aj1AhQ�Af�yAdI�A`��A_dZA^^5A\�yA[��AZ�AZ �AX��AV��AT  ARbNAQ�AK��AI��AF��AE;dAC�7AA�7A?��A<�jA8��A7��A7A5�7A4�A41A3�-A3oA17LA,��A+C�A*�!A*M�A)�A(��A(A%�FA$ �A"E�A �A�hA�AI�A��A�PAI�A��AVA��AJA�HA1AoA��A5?A��A��A�A%A��A�DA1A\)A �A
��A	A(�A�TA��A�!AA�A��AXAC�A+A�AM�A�;A33A�A�!AbNA�
A�A/AK�A Z@���@�5?@��j@� �@��\@���@���@�S�@�S�@��H@�-@��@���@�ȴ@�%@��@띲@�^5@�-@�  @�;d@�v�@�/@䛦@�  @�"�@���@�@� �@�|�@ޗ�@�x�@��@��@؋D@�5?@�O�@�I�@�\)@�@���@�M�@�p�@�z�@��@ΰ!@ͩ�@���@�r�@ˍP@�+@��@��@�ȴ@ʸR@���@�|�@�+@ʰ!@�{@�bN@Ƈ+@ŉ7@�%@�Ĝ@Ĭ@��@��@¸R@¸R@¸R@�v�@��@�p�@�7L@�V@��`@���@���@��P@��@���@�l�@�@��!@�^5@��^@��@��9@�Z@�  @��@��@��;@�C�@��y@�n�@�7L@���@���@��`@���@�z�@� �@�
=@��\@�M�@��#@��^@��h@��@��`@�b@���@�t�@�\)@�33@��H@�-@��T@��@�G�@�%@�j@�I�@� �@��F@�33@�ȴ@���@��+@��+@�@�@�X@�%@��9@�r�@�1@��P@�K�@�33@�@���@�v�@�J@��#@��-@�7L@���@�Ĝ@�Ĝ@�Ĝ@���@�1'@�ƨ@�l�@�C�@�+@��@�ȴ@�~�@�v�@�ff@�=q@���@��@�`B@�V@��/@���@�r�@�9X@�1@��@�|�@�;d@���@���@�E�@�J@���@��h@��@��`@��@���@�|�@�C�@���@���@���@��+@���@��#@��^@���@�Ĝ@��u@�bN@��F@�S�@�o@��@��!@��+@�ff@�-@���@�p�@�7L@��/@��D@�1'@��m@��P@�|�@��H@���@�V@��T@��^@��^@��h@�x�@�`B@�&�@��@��`@�Ĝ@��@�Z@�(�@���@���@��@�t�@�dZ@�K�@�
=@��H@���@���@�v�@�@�@���@�O�@��/@��u@�I�@���@��@�C�@��@��!@�E�@�$�@��@�{@�J@���@��#@���@�p�@�7L@���@��@���@���@��/@��9@�z�@�Q�@�I�@�(�@��@��@��F@���@�t�@�S�@�;d@�+@��6@z@h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bl�Bl�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�BiyBbNB`BB^5BVBR�BP�BO�BQ�BQ�BO�BO�BN�BR�BW
BW
BW
BVBT�BT�BS�BS�BQ�B[#B`BBgmBq�Bt�Bt�Bu�Bx�B|�B|�B}�B�B�B�%B�%B�B�B�B�B�B�B�1B��B!�B,B-B1'B49B8RBG�B]/BcTBffBiyBk�Bk�Bk�Bw�B~�B�Bz�B{�B|�By�BiyBS�BA�B'�B+B�B�B��BƨB�jB��B�1B~�Bt�B_;BZBYB\)BQ�B49B�BB
�HB
��B
ÖB
�RB
�B
��B
�bB
l�B
VB
J�B
/B
�B
PB	�B	��B	�jB	��B	�B	v�B	hsB	]/B	R�B	C�B	49B	-B	'�B	!�B	�B	�B	{B	bB	
=B	B��B��B�mB�BB�B��B��BɺBÖB�qB�RB�LB�FB�?B�9B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B�B�!B�'B�-B�9B�9B�9B�LB�XB�jB��BÖBÖBB��B��B��B�)B�/B�#B�;B�BB�HB�BB�ZB�`B�sB�yB�B�B�B�B�B�sB�TB�HB�`B�fB�`B�`B�sB�B�B�B�B��B��B��B��B��B��B��B	B��B��B��B	
=B	VB	hB	uB	uB	hB	hB	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	'�B	(�B	5?B	9XB	<jB	=qB	=qB	>wB	>wB	A�B	F�B	F�B	H�B	N�B	Q�B	S�B	S�B	XB	YB	XB	XB	XB	XB	\)B	_;B	_;B	dZB	jB	k�B	m�B	n�B	p�B	r�B	u�B	u�B	u�B	t�B	r�B	n�B	jB	jB	k�B	m�B	q�B	r�B	t�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�B	�%B	�=B	�\B	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�3B	�9B	�9B	�?B	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�qB	�wB	�}B	�}B	�}B	�}B	�}B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�)B	�/B	�BB	�BB	�;B	�;B	�BB	�HB	�NB	�TB	�`B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
bB
�B
(�B
1A22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   Bl�Bl�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�BiyBbNB`BB^5BVBR�BP�BO�BQ�BQ�BO�BO�BN�BR�BW
BW
BW
BVBT�BT�BS�BS�BQ�B[#B`BBgmBq�Bt�Bt�Bu�Bx�B|�B|�B}�B�B�B�%B�%B�B�B�B�B�B�B�1B��B!�B,B-B1'B49B8RBG�B]/BcTBffBiyBk�Bk�Bk�Bw�B~�B�Bz�B{�B|�By�BiyBS�BA�B'�B+B�B�B��BƨB�jB��B�1B~�Bt�B_;BZBYB\)BQ�B49B�BB
�HB
��B
ÖB
�RB
�B
��B
�bB
l�B
VB
J�B
/B
�B
PB	�B	��B	�jB	��B	�B	v�B	hsB	]/B	R�B	C�B	49B	-B	'�B	!�B	�B	�B	{B	bB	
=B	B��B��B�mB�BB�B��B��BɺBÖB�qB�RB�LB�FB�?B�9B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B�B�!B�'B�-B�9B�9B�9B�LB�XB�jB��BÖBÖBB��B��B��B�)B�/B�#B�;B�BB�HB�BB�ZB�`B�sB�yB�B�B�B�B�B�sB�TB�HB�`B�fB�`B�`B�sB�B�B�B�B��B��B��B��B��B��B��B	B��B��B��B	
=B	VB	hB	uB	uB	hB	hB	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	'�B	(�B	5?B	9XB	<jB	=qB	=qB	>wB	>wB	A�B	F�B	F�B	H�B	N�B	Q�B	S�B	S�B	XB	YB	XB	XB	XB	XB	\)B	_;B	_;B	dZB	jB	k�B	m�B	n�B	p�B	r�B	u�B	u�B	u�B	t�B	r�B	n�B	jB	jB	k�B	m�B	q�B	r�B	t�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�B	�%B	�=B	�\B	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�3B	�9B	�9B	�?B	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�qB	�wB	�}B	�}B	�}B	�}B	�}B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�)B	�/B	�BB	�BB	�;B	�;B	�BB	�HB	�NB	�TB	�`B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
bB
�B
(�B
1A22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190539                              AO  ARCAADJP                                                                    20181005190539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190539  QCF$                G�O�G�O�G�O�8000            