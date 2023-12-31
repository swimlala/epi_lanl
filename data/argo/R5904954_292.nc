CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:55Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191755  20181005191755  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              $A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�;�Z1   @��eUUh @5Q���l��dS���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     $A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C%�fC'�fC*  C,  C.�C0�C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CO�fCQ�fCT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch�Cj  Cl�Cn  Cp  Cr�Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C��3C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C��C��C��C�  C��3C�  C��C��C�  C��3C�  C��C��C�  C�  C��C��C��D fD �fD  D� DfD� DfD�fDfD� D��D� D  D� D�3D� D  D� D	fD	�fD	��D
� D  D� D  Dy�D  Dy�D  D�fD��D� D  D� DfD� D  Dy�D��Dy�D��D� DfD�fDfD�fDfD� D  D� DfD�fDfD� D��D� DfD� D��D� D  D� D  D�fD fD �fD!fD!�fD"  D"y�D"��D#� D$fD$�fD%  D%�fD&  D&y�D'  D'�fD(  D(y�D(��D)� D*fD*� D*��D+�fD,fD,y�D,��D-� D.  D.��D/  D/y�D0  D0�fD1fD1� D2fD2� D3  D3�fD4fD4�fD5  D5y�D6  D6� D7  D7�fD8fD8�fD9  D9�fD:fD:� D:��D;� D<fD<�fD<��D=� D>  D>� D?fD?y�D?��D@y�DA  DA� DB  DB� DC  DC� DC��DD� DE  DEy�DF  DFy�DF��DGy�DG��DH� DIfDI� DJ  DJy�DJ��DK� DLfDL� DMfDM� DM��DN� DOfDO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DT��DUy�DVfDV� DWfDW� DXfDX� DY  DY� DZ  DZy�D[  D[�fD\fD\�fD\��D]� D^  D^� D_fD_�fD`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Ddy�Dd��De� DffDf� Dg  Dg�fDh  Dh� Dh��Di� DjfDj�fDk  Dky�Dl  Dl� Dm  Dm��Dn�Dn�fDo  Do� DpfDp� Dq  Dq� Dr  Dry�Ds  Ds� Ds��Dt� Du  Du�fDvfDv� Dv��Dwl�Dyt{D�4�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�B\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)Bx��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮B�z�BԮBخBܮB�B�B��GB��GB��GB��B��B��C W
CW
CW
CW
CW
C
W
CW
C=pCW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&=pC(=pC*W
C,W
C.p�C0p�C2W
C4W
C6p�C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CH=pCJW
CLW
CNW
CP=pCR=pCTW
CVW
CXW
CZW
C\p�C^p�C`W
CbW
CdW
CfW
Chp�CjW
Clp�CnW
CpW
Crp�CtW
Cvp�CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�8RC�8RC�+�C�+�C��C�+�C�+�C��C��C��C��C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C��C��C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C��C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C�8RC�+�C�8RC�+�C�+�C�+�C�+�C�+�C�8RC�+�C��C�+�C�8RC�+�C��C�+�C�8RC�8RC�8RC�+�C��C�+�C�8RC�8RC�+�C��C�+�C�8RC�8RC�+�C�+�C�8RC�8RC�8RD )D �)D�D��D)D��D)D�)D)D��D]D��D�D��D�D��D�D��D	)D	�)D
]D
��D�D��D�D�]D�D�]D�D�)D]D��D�D��D)D��D�D�]D]D�]D]D��D)D�)D)D�)D)D��D�D��D)D�)D)D��D]D��D)D��D]D��D�D��D�D�)D )D �)D!)D!�)D"�D"�]D#]D#��D$)D$�)D%�D%�)D&�D&�]D'�D'�)D(�D(�]D)]D)��D*)D*��D+]D+�)D,)D,�]D-]D-��D.�D.��D/�D/�]D0�D0�)D1)D1��D2)D2��D3�D3�)D4)D4�)D5�D5�]D6�D6��D7�D7�)D8)D8�)D9�D9�)D:)D:��D;]D;��D<)D<�)D=]D=��D>�D>��D?)D?�]D@]D@�]DA�DA��DB�DB��DC�DC��DD]DD��DE�DE�]DF�DF�]DG]DG�]DH]DH��DI)DI��DJ�DJ�]DK]DK��DL)DL��DM)DM��DN]DN��DO)DO��DP]DP��DQ�DQ��DR�DR��DS�DS��DT�DT�]DU]DU�]DV)DV��DW)DW��DX)DX��DY�DY��DZ�DZ�]D[�D[�)D\)D\�)D]]D]��D^�D^��D_)D_�)D`�D`��Da�Da��Db�Db�]Dc�Dc��Dd�Dd�]De]De��Df)Df��Dg�Dg�)Dh�Dh��Di]Di��Dj)Dj�)Dk�Dk�]Dl�Dl��Dm�Dm��Dn"�Dn�)Do�Do��Dp)Dp��Dq�Dq��Dr�Dr�]Ds�Ds��Dt]Dt��Du�Du�)Dv)Dv��Dw]Dw��Dy�>D�?�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴAß�A���A�JA�1'A��PA�;dA�XA�ȴA��
A��#A��mA��#A�l�A�bNA�M�A�;dA�33A� �A�oA�
=A��A��mA��#A�ȴA��9A��A���A��uA��+A�|�A�n�A�^5A�I�A�bA��TA���A�S�A�+A�oA��9A�E�A��;A���A��wA��PA�oA���A�t�A�C�A�"�A��A�`BA�$�A���A���A�p�A�jA�G�A��A�$�A�l�A�bA��hA�&�A��RA��PA�bA��RA��\A��FA�?}A��A�%A���A�=qA��HA���A���A�"�A�I�A��RA�?}A�E�A�E�A�1'A��DA��;A�  A�K�A���A�E�A��A��TA��A�dZA�dZA��^A�/A�XA�G�A��HA�  A��hA�v�A���A���A��\A��;A�/A�ƨA�/A��hA~ȴA{�^Az��Ax��Ax9XAw/Au�AuVAq�mAp��Ao��An�DAm��AlE�Aj�Ail�AhA�Ag�Ag��Ag?}Af��Af�AfAc��AbQ�AbbA_��A^�9A^=qA]��A]l�A\�`A\��A\bNA[AX�AV1'AT9XARI�AP��AM&�AKp�AK;dAJ��AI��AI+AG`BAD�\AB~�AAt�A@�A@z�A?��A?C�A?%A>�+A=�FA=O�A;�7A9oA7/A5�A4bNA4$�A4bA2��A/C�A-VA*�A*�A*A�A'K�A&-A%��A%��A$�yA#��A"��A"bNA!��A ��A ffA�DAn�A��AI�A�-A��A �AXA�PA7LA��A?}AbNA�A�TA
A�A	A�uA�PA`BA?}A��A��A9XA\)AjA�wA33A bN@�|�@�V@���@�Z@�j@��m@��@��\@��
@�|�@�C�@�o@�
=@��y@���@���@��m@�33@�7L@� �@�K�@홚@�9@웦@�D@�z�@��;@��H@�O�@�Q�@�ȴ@�p�@��;@��;@���@���@���@�Ĝ@�(�@ӕ�@�t�@�@�-@щ7@�p�@��@��H@�%@��@��@�O�@ț�@ǝ�@��y@�=q@ģ�@�+@�=q@�p�@�7L@��`@�(�@�ƨ@��@�S�@��@���@�V@��@�1@��@�v�@��@��@�dZ@�n�@��7@���@�Z@��F@�33@��H@�^5@���@��@�V@��@�j@�l�@��H@���@��y@�K�@�K�@�
=@�ȴ@�=q@��h@�%@�Q�@�9X@�1'@��
@�S�@���@���@��@�$�@��@�V@��`@���@���@�z�@�1'@��@���@�t�@�C�@�"�@��@��!@�n�@���@���@��h@�p�@�G�@���@�Ĝ@�z�@��@��P@�K�@�+@��@�ȴ@�ff@�@���@���@���@���@�x�@��@���@�Q�@���@��P@�33@�ȴ@���@�n�@��#@��@��@�J@���@��T@��h@�V@�Q�@�b@���@�|�@�t�@�t�@�|�@�dZ@�S�@�;d@��@��y@��@�ȴ@�v�@�$�@���@��@��@�  @��@�;d@�"�@���@��y@��+@�J@�J@���@�X@���@��`@��@�z�@�b@��F@���@��@�t�@�;d@��@��R@���@���@���@�5?@��@��h@�X@�O�@�G�@�&�@��@��@���@�z�@�j@�A�@�b@��m@��
@��F@���@�|�@�K�@��H@��R@��\@�=q@�{@�@��-@���@�hs@�?}@�/@�V@���@��/@���@�Z@�b@���@��P@�t�@�S�@�33@��@���@��@���@���@��H@���@��!@���@���@��+@�ff@�E�@��@���@���@�@��h@�/@�/@��@���@��@~4@lU211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ȴAß�A���A�JA�1'A��PA�;dA�XA�ȴA��
A��#A��mA��#A�l�A�bNA�M�A�;dA�33A� �A�oA�
=A��A��mA��#A�ȴA��9A��A���A��uA��+A�|�A�n�A�^5A�I�A�bA��TA���A�S�A�+A�oA��9A�E�A��;A���A��wA��PA�oA���A�t�A�C�A�"�A��A�`BA�$�A���A���A�p�A�jA�G�A��A�$�A�l�A�bA��hA�&�A��RA��PA�bA��RA��\A��FA�?}A��A�%A���A�=qA��HA���A���A�"�A�I�A��RA�?}A�E�A�E�A�1'A��DA��;A�  A�K�A���A�E�A��A��TA��A�dZA�dZA��^A�/A�XA�G�A��HA�  A��hA�v�A���A���A��\A��;A�/A�ƨA�/A��hA~ȴA{�^Az��Ax��Ax9XAw/Au�AuVAq�mAp��Ao��An�DAm��AlE�Aj�Ail�AhA�Ag�Ag��Ag?}Af��Af�AfAc��AbQ�AbbA_��A^�9A^=qA]��A]l�A\�`A\��A\bNA[AX�AV1'AT9XARI�AP��AM&�AKp�AK;dAJ��AI��AI+AG`BAD�\AB~�AAt�A@�A@z�A?��A?C�A?%A>�+A=�FA=O�A;�7A9oA7/A5�A4bNA4$�A4bA2��A/C�A-VA*�A*�A*A�A'K�A&-A%��A%��A$�yA#��A"��A"bNA!��A ��A ffA�DAn�A��AI�A�-A��A �AXA�PA7LA��A?}AbNA�A�TA
A�A	A�uA�PA`BA?}A��A��A9XA\)AjA�wA33A bN@�|�@�V@���@�Z@�j@��m@��@��\@��
@�|�@�C�@�o@�
=@��y@���@���@��m@�33@�7L@� �@�K�@홚@�9@웦@�D@�z�@��;@��H@�O�@�Q�@�ȴ@�p�@��;@��;@���@���@���@�Ĝ@�(�@ӕ�@�t�@�@�-@щ7@�p�@��@��H@�%@��@��@�O�@ț�@ǝ�@��y@�=q@ģ�@�+@�=q@�p�@�7L@��`@�(�@�ƨ@��@�S�@��@���@�V@��@�1@��@�v�@��@��@�dZ@�n�@��7@���@�Z@��F@�33@��H@�^5@���@��@�V@��@�j@�l�@��H@���@��y@�K�@�K�@�
=@�ȴ@�=q@��h@�%@�Q�@�9X@�1'@��
@�S�@���@���@��@�$�@��@�V@��`@���@���@�z�@�1'@��@���@�t�@�C�@�"�@��@��!@�n�@���@���@��h@�p�@�G�@���@�Ĝ@�z�@��@��P@�K�@�+@��@�ȴ@�ff@�@���@���@���@���@�x�@��@���@�Q�@���@��P@�33@�ȴ@���@�n�@��#@��@��@�J@���@��T@��h@�V@�Q�@�b@���@�|�@�t�@�t�@�|�@�dZ@�S�@�;d@��@��y@��@�ȴ@�v�@�$�@���@��@��@�  @��@�;d@�"�@���@��y@��+@�J@�J@���@�X@���@��`@��@�z�@�b@��F@���@��@�t�@�;d@��@��R@���@���@���@�5?@��@��h@�X@�O�@�G�@�&�@��@��@���@�z�@�j@�A�@�b@��m@��
@��F@���@�|�@�K�@��H@��R@��\@�=q@�{@�@��-@���@�hs@�?}@�/@�V@���@��/@���@�Z@�b@���@��P@�t�@�S�@�33@��@���@��@���@���@��H@���@��!@���@���@��+@�ff@�E�@��@���@���@�@��h@�/@�/@��@���@��@~4@lU211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B>wBB�BD�BH�BJ�BVB[#B]/BiyBq�Bt�B}�B�B� B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B�B�B�B�B�!B�9B�qBŢB��B��B��BȴBÖBĜBB�wB��B��B��B�{B��B��B�oB�\B�B}�Bx�Br�Bp�Bm�BffBZBP�BA�B6FB-B#�B�B{BDB��B�yB�#BǮB��B�JBl�BE�B.B�BPBDB	7BB
��B
��B
�B
�
B
ȴB
�}B
�RB
�-B
�B
��B
�uB
x�B
o�B
]/B
XB
P�B
G�B
?}B
+B
"�B
�B
uB
PB
B	��B	�B	�B	�B	�sB	�fB	�TB	�HB	�/B	��B	ƨB	ÖB	�LB	�B	�B	��B	��B	��B	��B	��B	��B	�1B	t�B	e`B	ZB	N�B	;dB	1'B	/B	+B	$�B	�B	PB��B�B��B��B�B�B�B�B�B��B�B�mB�B��BƨB�jB�dB�XB�-B��B��B�uB�hB�PB�B~�B}�B|�Bz�Bx�Bw�Bw�Bw�Bx�Bw�Bx�Bz�B}�B|�B{�Bx�Bv�Bs�Br�Bo�Bm�Bk�BhsBgmBe`BbNB^5B\)B_;B^5B]/B\)B\)BZBYBZB[#B[#B]/B]/B\)B^5BaHBiyBl�Bo�Br�Bv�Bv�Bw�Bw�Bw�Bw�Bv�Bv�Bw�Bw�By�Bz�B{�B~�B~�B~�B}�B}�B}�B}�B|�B{�B{�By�Bx�Bp�BjBo�Bt�Bx�B|�B�B�B�B�1B�JB�JB�PB��B��B��B��B��B��B��B�B�B�!B�?B�RB�dB�jB�qBÖBŢBŢBǮBǮBǮBǮBȴBɺBȴBɺBɺB��B��B��B��B��B��B�B�
B�B�B�/B�;B�TB�ZB�sB�B�B��B��B	B	+B	1B		7B	PB	{B	�B	�B	�B	!�B	$�B	&�B	(�B	+B	+B	0!B	7LB	9XB	:^B	:^B	=qB	>wB	?}B	A�B	D�B	F�B	G�B	I�B	K�B	M�B	O�B	VB	XB	XB	YB	[#B	^5B	`BB	cTB	e`B	hsB	iyB	jB	jB	k�B	n�B	q�B	r�B	r�B	r�B	s�B	x�B	}�B	�B	�B	�B	� B	�B	�7B	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�RB	�^B	�jB	�qB	�wB	��B	��B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
%B
�B
�B
+k22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B>wBB�BD�BH�BJ�BVB[#B]/BiyBq�Bt�B}�B�B� B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B�B�B�B�B�!B�9B�qBŢB��B��B��BȴBÖBĜBB�wB��B��B��B�{B��B��B�oB�\B�B}�Bx�Br�Bp�Bm�BffBZBP�BA�B6FB-B#�B�B{BDB��B�yB�#BǮB��B�JBl�BE�B.B�BPBDB	7BB
��B
��B
�B
�
B
ȴB
�}B
�RB
�-B
�B
��B
�uB
x�B
o�B
]/B
XB
P�B
G�B
?}B
+B
"�B
�B
uB
PB
B	��B	�B	�B	�B	�sB	�fB	�TB	�HB	�/B	��B	ƨB	ÖB	�LB	�B	�B	��B	��B	��B	��B	��B	��B	�1B	t�B	e`B	ZB	N�B	;dB	1'B	/B	+B	$�B	�B	PB��B�B��B��B�B�B�B�B�B��B�B�mB�B��BƨB�jB�dB�XB�-B��B��B�uB�hB�PB�B~�B}�B|�Bz�Bx�Bw�Bw�Bw�Bx�Bw�Bx�Bz�B}�B|�B{�Bx�Bv�Bs�Br�Bo�Bm�Bk�BhsBgmBe`BbNB^5B\)B_;B^5B]/B\)B\)BZBYBZB[#B[#B]/B]/B\)B^5BaHBiyBl�Bo�Br�Bv�Bv�Bw�Bw�Bw�Bw�Bv�Bv�Bw�Bw�By�Bz�B{�B~�B~�B~�B}�B}�B}�B}�B|�B{�B{�By�Bx�Bp�BjBo�Bt�Bx�B|�B�B�B�B�1B�JB�JB�PB��B��B��B��B��B��B��B�B�B�!B�?B�RB�dB�jB�qBÖBŢBŢBǮBǮBǮBǮBȴBɺBȴBɺBɺB��B��B��B��B��B��B�B�
B�B�B�/B�;B�TB�ZB�sB�B�B��B��B	B	+B	1B		7B	PB	{B	�B	�B	�B	!�B	$�B	&�B	(�B	+B	+B	0!B	7LB	9XB	:^B	:^B	=qB	>wB	?}B	A�B	D�B	F�B	G�B	I�B	K�B	M�B	O�B	VB	XB	XB	YB	[#B	^5B	`BB	cTB	e`B	hsB	iyB	jB	jB	k�B	n�B	q�B	r�B	r�B	r�B	s�B	x�B	}�B	�B	�B	�B	� B	�B	�7B	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�RB	�^B	�jB	�qB	�wB	��B	��B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
%B
�B
�B
+k22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191755                              AO  ARCAADJP                                                                    20181005191755    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191755  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191755  QCF$                G�O�G�O�G�O�8000            