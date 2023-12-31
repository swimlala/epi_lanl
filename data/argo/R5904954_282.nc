CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:53Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  JH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Yp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  yp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191753  20181005191753  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��� a)h1   @�����@5St�j~��d}�7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @,��@�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC�fC�fC  C  C  C  C  C  C  C   C"  C$�C&�C(  C*�C,  C.  C0  C2  C4�C6  C8  C9�fC<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C��C��C�  C��3C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��3C��3C��3C��fC��3C��3D   D � D  Dy�D��D� D  Dy�D  Dy�D��D� DfD� D��D� D  D� D	  D	�fD
  D
y�D
��Dy�D��D� D  D� D  D� DfD�fDfD�fDfD�fDfD� D��D� DfD� D  Dy�D  D�fDfD� D  D� D  D�fDfD� D  D� D��D� D��Ds3D��D� DfD�fD   D y�D ��D!y�D!��D"� D#  D#� D$fD$� D%  D%�fD%��D&� D'  D'�fD(  D(y�D)fD)� D)��D*y�D+  D+� D+�3D,y�D-  D-�fD.  D.� D/fD/� D0  D0� D1  D1� D1��D2y�D3fD3�fD4�D4�fD5  D5� D6fD6y�D6��D7y�D8  D8�fD9fD9� D:  D:�fD;  D;� D<fD<�fD=fD=� D>  D>�fD?  D?y�D@fD@� DA  DA� DBfDB� DC  DC� DD  DD� DD��DE�fDF  DF� DGfDG� DG��DHy�DI  DI� DJ  DJ�fDJ��DK�fDLfDL� DMfDM� DM��DN� DOfDO� DO��DP� DQ  DQ� DRfDRy�DR��DS� DT  DT� DUfDU� DV  DV�fDW  DW� DW��DXy�DX��DYy�DZfDZ�fD[fD[� D\fD\� D\��D]� D^  D^�fD_fD_� D_��D`y�D`��Da� Db  Db�fDb��Dcy�Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj�fDkfDk� Dl  Dl�fDmfDm��DnfDn�fDo  Doy�Dp  Dp� Dq  Dq�fDr  Dry�Dr��Ds� DtfDt�fDu  Du� Dv  Dv�fDw  Dwy�Dw� Dy��D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @B�\@�{@��HAp�A%p�AC�
Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)B@��BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B�z�B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B��GB��GB�B�B��B��B��C W
CW
CW
CW
CW
C
W
C=pC=pC=pCW
CW
CW
CW
CW
CW
CW
C W
C"W
C$p�C&p�C(W
C*p�C,W
C.W
C0W
C2W
C4p�C6W
C8W
C:=pC<W
C>W
C@W
CBW
CDW
CF=pCHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`p�CbW
CdW
CfW
ChW
CjW
ClW
Cnp�CpW
CrW
Ctp�CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C��C�+�C�+�C�+�C�8RC�8RC�8RC�+�C��C��C�+�C�8RC�8RC�+�C�+�C�+�C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�8RC��C��C��C��C��C��D �D ��D�D�]D]D��D�D�]D�D�]D]D��D)D��D]D��D�D��D	�D	�)D
�D
�]D]D�]D]D��D�D��D�D��D)D�)D)D�)D)D�)D)D��D]D��D)D��D�D�]D�D�)D)D��D�D��D�D�)D)D��D�D��D]D��D]D��D]D��D)D�)D �D �]D!]D!�]D"]D"��D#�D#��D$)D$��D%�D%�)D&]D&��D'�D'�)D(�D(�]D))D)��D*]D*�]D+�D+��D,�D,�]D-�D-�)D.�D.��D/)D/��D0�D0��D1�D1��D2]D2�]D3)D3�)D4"�D4�)D5�D5��D6)D6�]D7]D7�]D8�D8�)D9)D9��D:�D:�)D;�D;��D<)D<�)D=)D=��D>�D>�)D?�D?�]D@)D@��DA�DA��DB)DB��DC�DC��DD�DD��DE]DE�)DF�DF��DG)DG��DH]DH�]DI�DI��DJ�DJ�)DK]DK�)DL)DL��DM)DM��DN]DN��DO)DO��DP]DP��DQ�DQ��DR)DR�]DS]DS��DT�DT��DU)DU��DV�DV�)DW�DW��DX]DX�]DY]DY�]DZ)DZ�)D[)D[��D\)D\��D]]D]��D^�D^�)D_)D_��D`]D`�]Da]Da��Db�Db�)Dc]Dc�]Dd�Dd��De�De��Df]Df��Dg�Dg��Dh�Dh��Di)Di��Dj�Dj�)Dk)Dk��Dl�Dl�)Dm)Dm��Dn)Dn�)Do�Do�]Dp�Dp��Dq�Dq�)Dr�Dr�]Ds]Ds��Dt)Dt�)Du�Du��Dv�Dv�)Dw�Dw�]Dw��Dy�RD�J�D�Ǯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A��A��A���A��A��A���A���A���A���A���A�jA� �A��A���A��PA��+A�~�A�hsA�`BA�S�A�C�A�33A�1A��A��A�v�A�p�A�`BA�(�A��A���A��A�ȴA�|�A�I�A�(�A�{A�A��A���A���A�`BA�I�A��FA��mA�XA�oA�S�A�n�A���A�\)A�-A��yA�$�A�x�A�Q�A��A��A�M�A�ĜA�A�A�v�A��A�VA�M�A���A�/A��
A��A�v�A��A�ȴA�JA�?}A�ȴA�?}A���A���A��`A�bNA��hA���A�&�A��A�/A�|�A�ȴA�^5A���A�x�A�K�A�~�A�(�A��A�I�A�-A��yA��7A�9XA���A�v�A�1'A��A�&�A��A�z�A��PA�ƨA���A��\A�VA��;A�jA��yA�A~1A|�AzĜAzJAy��As��Ap�Ao��Aol�Ao33AnI�AmAl�!Ah�DAf��Ae+Ac&�A`��A[`BAY��AXn�AWXAV��AV�AU��AT��A�A��AJA��A�A��A��A�A�Az�A�wA/A��A-AJA|�A"�A
1'A	��Ar�A�wA-A��A�FAv�A33@��@��T@�33@���@�  @�\)@���@�-@�1'@�V@�C�@�7L@�9X@�@�z�@�C�@�R@��T@�@�z�@އ+@�r�@�^5@ٲ-@�&�@ؓu@��;@��H@��@Ԭ@ԃ@�Z@�I�@���@�V@�J@�%@У�@�  @Ϯ@���@���@�X@��`@�A�@�;d@ʗ�@���@�hs@��;@�@ư!@�5?@�/@�z�@� �@��m@�O�@� �@�S�@�"�@��-@��@�bN@�  @�S�@�ȴ@�n�@��T@���@�O�@�V@��@�bN@��@�{@�&�@���@�bN@�  @��@�
=@�
=@��@�o@��@�`B@���@���@��w@���@��@��`@�1'@��P@�dZ@�;d@�
=@��+@�E�@�^5@��@��7@��@�Ĝ@�9X@��@��F@�;d@�+@���@�~�@�ff@�5?@��T@�`B@��j@�9X@��
@�@��R@�ff@�^5@�ff@�ff@�$�@��#@���@�hs@�7L@���@��`@���@�z�@���@���@���@��@���@���@��@�o@���@�ȴ@��@���@���@��\@��\@��+@�n�@�V@�M�@�=q@�-@��@��@��m@��
@�|�@�t�@�33@�
=@��y@���@�ȴ@���@�v�@�ff@�^5@��@���@���@�/@�?}@��h@�V@�Z@��;@���@�ƨ@��w@��F@��F@��F@��F@���@��@�+@�n�@�J@��^@�/@��@��j@�z�@�Z@�(�@��
@��@�"�@�@��y@��H@��@��!@�V@�J@���@��h@���@���@��T@��@��7@�`B@�O�@���@�j@��P@�"�@�@��@��R@��!@�v�@�V@�$�@�@���@�@���@�G�@�O�@�?}@��@�V@�%@�Ĝ@�Z@�I�@�9X@��m@��F@���@�l�@�"�@��y@��R@��!@���@�ff@�$�@��@�p�@�?}@���@��9@��@{��@is�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��A��A���A��A��A���A���A���A���A���A�jA� �A��A���A��PA��+A�~�A�hsA�`BA�S�A�C�A�33A�1A��A��A�v�A�p�A�`BA�(�A��A���A��A�ȴA�|�A�I�A�(�A�{A�A��A���A���A�`BA�I�A��FA��mA�XA�oA�S�A�n�A���A�\)A�-A��yA�$�A�x�A�Q�A��A��A�M�A�ĜA�A�A�v�A��A�VA�M�A���A�/A��
A��A�v�A��A�ȴA�JA�?}A�ȴA�?}A���A���A��`A�bNA��hA���A�&�A��A�/A�|�A�ȴA�^5A���A�x�A�K�A�~�A�(�A��A�I�A�-A��yA��7A�9XA���A�v�A�1'A��A�&�A��A�z�A��PA�ƨA���A��\A�VA��;A�jA��yA�A~1A|�AzĜAzJAy��As��Ap�Ao��Aol�Ao33AnI�AmAl�!Ah�DAf��Ae+Ac&�A`��A[`BAY��AXn�AWXAV��AV�AU��AT��A�A��AJA��A�A��A��A�A�Az�A�wA/A��A-AJA|�A"�A
1'A	��Ar�A�wA-A��A�FAv�A33@��@��T@�33@���@�  @�\)@���@�-@�1'@�V@�C�@�7L@�9X@�@�z�@�C�@�R@��T@�@�z�@އ+@�r�@�^5@ٲ-@�&�@ؓu@��;@��H@��@Ԭ@ԃ@�Z@�I�@���@�V@�J@�%@У�@�  @Ϯ@���@���@�X@��`@�A�@�;d@ʗ�@���@�hs@��;@�@ư!@�5?@�/@�z�@� �@��m@�O�@� �@�S�@�"�@��-@��@�bN@�  @�S�@�ȴ@�n�@��T@���@�O�@�V@��@�bN@��@�{@�&�@���@�bN@�  @��@�
=@�
=@��@�o@��@�`B@���@���@��w@���@��@��`@�1'@��P@�dZ@�;d@�
=@��+@�E�@�^5@��@��7@��@�Ĝ@�9X@��@��F@�;d@�+@���@�~�@�ff@�5?@��T@�`B@��j@�9X@��
@�@��R@�ff@�^5@�ff@�ff@�$�@��#@���@�hs@�7L@���@��`@���@�z�@���@���@���@��@���@���@��@�o@���@�ȴ@��@���@���@��\@��\@��+@�n�@�V@�M�@�=q@�-@��@��@��m@��
@�|�@�t�@�33@�
=@��y@���@�ȴ@���@�v�@�ff@�^5@��@���@���@�/@�?}@��h@�V@�Z@��;@���@�ƨ@��w@��F@��F@��F@��F@���@��@�+@�n�@�J@��^@�/@��@��j@�z�@�Z@�(�@��
@��@�"�@�@��y@��H@��@��!@�V@�J@���@��h@���@���@��T@��@��7@�`B@�O�@���@�j@��P@�"�@�@��@��R@��!@�v�@�V@�$�@�@���@�@���@�G�@�O�@�?}@��@�V@�%@�Ĝ@�Z@�I�@�9X@��m@��F@���@�l�@�"�@��y@��R@��!@���@�ff@�$�@��@�p�@�?}@���@��9@��@{��@is�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYBXBXBXBXBXBXBXBXBXBXBW
BW
BVBVBYBaHBffBiyBm�Bu�Bx�B|�B�B�B�=B��B��B��B��B��B��B��B��B�B�3B�qB��BBBBÖBĜBȴB��B��B��B��B�5B�BB�5B�)B�B�B��B��B��B��B��B��B��BȴBB�jB�9B�3B�jB�wB�}BǮBŢB��B� Bp�BbNBQ�BJ�BE�B@�B;dB1'B%�B�BPB  B��B�B�`B�B��B�jB�FB�'B��B�uB~�BgmBF�B49B0!B)�B$�B�B
��B
�B
�sB
�mB
�TB
�5B
�
B
��B
�B
�B
��B
��B
ŢB
�-B
��B
�PB
�B
v�B
o�B
hsB
@�B
(�B
"�B
�B
�B
�B
bB
B	�yB	�#B	��B	��B	�B	�DB	�B	{�B	v�B	t�B	r�B	n�B	iyB|�Bz�By�Bx�Bx�By�Bx�Bx�By�By�Bx�Bw�Bw�Bw�Bv�Bv�Bu�Bt�Br�Br�Br�Bq�Bn�Bk�BjBffBe`BdZBffBgmBhsBhsBhsBgmBiyBk�Bl�Bn�Bn�Bp�Bq�Bs�Bs�Bs�Bs�Bs�Bt�Bv�B{�B|�B~�B� B�B�B�B�7B�=B�DB�=B�\B�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�?B�9B�}BĜBȴBȴB��B��B�B�
B�B�B�#B�;B�TB�mB�sB�B�B�B�B��B��B��B��B��B��B	B	B		7B	PB	VB	\B	oB	�B	�B	�B	"�B	'�B	(�B	+B	-B	.B	0!B	1'B	5?B	9XB	:^B	<jB	=qB	?}B	A�B	C�B	E�B	G�B	L�B	M�B	N�B	P�B	R�B	VB	ZB	\)B	\)B	\)B	_;B	bNB	dZB	e`B	gmB	jB	l�B	m�B	o�B	p�B	r�B	v�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�JB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�9B	�9B	�9B	�FB	�LB	�LB	�RB	�^B	�dB	�dB	�dB	�jB	�}B	��B	B	ÖB	ÖB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�TB	�NB	�TB	�TB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
+B
tB
dB
-C22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BYBXBXBXBXBXBXBXBXBXBXBW
BW
BVBVBYBaHBffBiyBm�Bu�Bx�B|�B�B�B�=B��B��B��B��B��B��B��B��B�B�3B�qB��BBBBÖBĜBȴB��B��B��B��B�5B�BB�5B�)B�B�B��B��B��B��B��B��B��BȴBB�jB�9B�3B�jB�wB�}BǮBŢB��B� Bp�BbNBQ�BJ�BE�B@�B;dB1'B%�B�BPB  B��B�B�`B�B��B�jB�FB�'B��B�uB~�BgmBF�B49B0!B)�B$�B�B
��B
�B
�sB
�mB
�TB
�5B
�
B
��B
�B
�B
��B
��B
ŢB
�-B
��B
�PB
�B
v�B
o�B
hsB
@�B
(�B
"�B
�B
�B
�B
bB
B	�yB	�#B	��B	��B	�B	�DB	�B	{�B	v�B	t�B	r�B	n�B	iyB|�Bz�By�Bx�Bx�By�Bx�Bx�By�By�Bx�Bw�Bw�Bw�Bv�Bv�Bu�Bt�Br�Br�Br�Bq�Bn�Bk�BjBffBe`BdZBffBgmBhsBhsBhsBgmBiyBk�Bl�Bn�Bn�Bp�Bq�Bs�Bs�Bs�Bs�Bs�Bt�Bv�B{�B|�B~�B� B�B�B�B�7B�=B�DB�=B�\B�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�?B�9B�}BĜBȴBȴB��B��B�B�
B�B�B�#B�;B�TB�mB�sB�B�B�B�B��B��B��B��B��B��B	B	B		7B	PB	VB	\B	oB	�B	�B	�B	"�B	'�B	(�B	+B	-B	.B	0!B	1'B	5?B	9XB	:^B	<jB	=qB	?}B	A�B	C�B	E�B	G�B	L�B	M�B	N�B	P�B	R�B	VB	ZB	\)B	\)B	\)B	_;B	bNB	dZB	e`B	gmB	jB	l�B	m�B	o�B	p�B	r�B	v�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�JB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�9B	�9B	�9B	�FB	�LB	�LB	�RB	�^B	�dB	�dB	�dB	�jB	�}B	��B	B	ÖB	ÖB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�TB	�NB	�TB	�TB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
+B
tB
dB
-C22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191753                              AO  ARCAADJP                                                                    20181005191753    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191753  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191753  QCF$                G�O�G�O�G�O�8000            