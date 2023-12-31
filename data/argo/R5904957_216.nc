CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:45Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181024140845  20181024140845  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��{��1   @��`�V@5 A�7K��d��
=q1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A���A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�fC�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D��D� D  D� D  Dy�D��D� D	fD	� D
  D
� D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D$  D$y�D$��D%� D%��D&� D'  D'� D(fD(�fD)fD)�fD*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/fD/�fD0  D0� D1  D1� D2  D2�fD3  D3� D4fD4�fD5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DN��DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DVfDV�fDW  DW� DW��DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Dsy�Dt  Dt� Dt��Du� Dv  Dvy�Dv��Dwy�DwٚDy��D�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@7
=@��@��A ��A ��A@��A`��A�z�A�G�A�z�A�z�A�z�A�G�A�z�A�z�B =qB=qB�B=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBg�Bp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\CL\CN(�CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cy��C|\C~\C��C��C�{C��C��C��C��C��C��C��C��C�{C�{C��C��C�{C��C��C��C��C�{C��C��C��C��C���C��C��C��C��C��C��C��C���C��C��C��C��C��C��C���C��C��C�{C��C��C��C��C��C�{C��C�{C�{C��C��C��C�{C��C��C��C��C�{C��C��C��C��C��C�{C�{C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C���C���C���C��C��C��C��C���C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D�=D�D��D�qD��D�D��D�D}qD�qD��D	
=D	��D
�D
��D�D��D�D}qD�D��D�D}qD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�qD��D�D$�D$}qD$�qD%��D%�qD&��D'�D'��D(
=D(�=D)
=D)�=D*�D*��D+�D+��D,
=D,��D-�D-��D.�D.��D/
=D/�=D0�D0��D1�D1��D2�D2�=D3�D3��D4
=D4�=D5�D5��D5�qD6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D<�qD=��D>�D>��D?�D?��D@�D@��DA
=DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DG�qDH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN}qDN�qDO��DP�DP��DQ�DQ��DR�DR��DR�qDS��DT�DT��DU�DU��DV
=DV�=DW�DW��DW�qDX}qDY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_�=D`�D`��Da�Da�=Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dq�qDr��Ds�Ds}qDt�Dt��Dt�qDu��Dv�Dv}qDv�qDw}qDw�qDy��D�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA��A��A�{A�oA�{A��A��A��A��A��A��A�"�A�$�A�$�A�(�A�+A�/A�33A�1'A�+A�+A��A��Aȇ+AǕ�AƾwA�;dAŸRA�t�A�ZA�?}A��TA�S�A�ȴA�I�A�1A���A��/A��A��A�r�A��DA�O�A�C�A���A�bA�A�A���A���A��A�jA�  A��!A���A�`BA� �A���A�A�$�A�VA�hsA�ZA��7A���A�9XA� �A��A�bA�dZA�z�A��\A��A��\A�oA���A��#A���A���A�z�A��TA�~�A��mA���A�v�A�p�A�7Az�Ay�Aw33Au�wAs�PAq?}Ao��AnVAl�Aj5?Ag�-Af��Ad�uAb��AaA`9XA_��A_
=A^�A^�/A^Q�A[�AZv�AY�wAXA�AUƨAS�^AQ�TAQ��AQx�AQS�APz�AO\)AM�AK�^AI��AH�RAE��ACdZAA�mA>M�A<��A<v�A<  A;�^A;A9�A8��A8�A6�HA3�A0�/A0�A/��A.9XA*�RA(M�A'�A'
=A&n�A%�mA%oA#A!�^A|�A�
AĜA�jA�9A�!A�DAE�A�7AAƨA�hA��AffA�A��AffAE�A��A�RA�7A��AM�AAl�A�At�A
�A	�#A	�-A	��A	�A	�A��A��A�A"�AoAI�A��AoA ��A r�A b@�t�@�Q�@��@���@�1@���@�@�w@�v�@�E�@�-@�?}@�9X@�"�@�o@�V@�C�@�(�@�~�@��@��@���@�@�Ĝ@�;d@��T@�@�V@�A�@ۅ@�^5@�V@�z�@���@׍P@��@��T@�I�@�K�@��@Ѳ-@ϝ�@��`@�5?@Ȭ@ȃ@��/@��`@�X@�Q�@ǍP@��`@���@ț�@�t�@Ł@���@�Ĝ@�Z@���@�~�@���@���@��!@�p�@�X@�G�@�/@��@�%@��@�9X@���@�33@���@�M�@�p�@��m@�"�@�33@�33@�@�v�@�;d@���@��m@��9@��@�J@���@�@��h@�p�@�X@�G�@�?}@�/@��@�V@���@��/@��9@�(�@��m@��
@�ƨ@���@�
=@��@�v�@�J@��#@��@�?}@�7L@���@�(�@��!@�{@��T@�@���@��h@���@�j@���@�S�@�C�@�33@�o@���@�ff@�M�@�-@��@�@��@���@�Ĝ@�z�@�(�@��m@��
@�"�@�~�@�M�@�{@��@�G�@��@�Ĝ@�j@��@��@��@��F@���@�|�@�+@�o@�
=@���@�{@�@�?}@���@��9@��9@���@���@�33@��y@��+@�-@���@���@�/@���@��`@��9@�r�@��P@�
=@�ȴ@���@�^5@�5?@��@�{@�M�@���@���@��@�^5@��#@��@��T@���@���@�V@���@�l�@�"�@�"�@��@�C�@�-@�n�@��H@�+@�
=@��y@��y@���@�~�@�~�@�~�@�-@�V@�v�@�n�@�V@�=q@�V@�E�@�-@���@���@��-@�hs@��`@�r�@�1'@��@�+@��@���@�=q@���@��-@��-@���@�O�@���@�V@�&�@�X@�hs@�/@��u@�Z@�9X@� �@���@��@��k@zE�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA��A��A�{A�oA�{A��A��A��A��A��A��A�"�A�$�A�$�A�(�A�+A�/A�33A�1'A�+A�+A��A��Aȇ+AǕ�AƾwA�;dAŸRA�t�A�ZA�?}A��TA�S�A�ȴA�I�A�1A���A��/A��A��A�r�A��DA�O�A�C�A���A�bA�A�A���A���A��A�jA�  A��!A���A�`BA� �A���A�A�$�A�VA�hsA�ZA��7A���A�9XA� �A��A�bA�dZA�z�A��\A��A��\A�oA���A��#A���A���A�z�A��TA�~�A��mA���A�v�A�p�A�7Az�Ay�Aw33Au�wAs�PAq?}Ao��AnVAl�Aj5?Ag�-Af��Ad�uAb��AaA`9XA_��A_
=A^�A^�/A^Q�A[�AZv�AY�wAXA�AUƨAS�^AQ�TAQ��AQx�AQS�APz�AO\)AM�AK�^AI��AH�RAE��ACdZAA�mA>M�A<��A<v�A<  A;�^A;A9�A8��A8�A6�HA3�A0�/A0�A/��A.9XA*�RA(M�A'�A'
=A&n�A%�mA%oA#A!�^A|�A�
AĜA�jA�9A�!A�DAE�A�7AAƨA�hA��AffA�A��AffAE�A��A�RA�7A��AM�AAl�A�At�A
�A	�#A	�-A	��A	�A	�A��A��A�A"�AoAI�A��AoA ��A r�A b@�t�@�Q�@��@���@�1@���@�@�w@�v�@�E�@�-@�?}@�9X@�"�@�o@�V@�C�@�(�@�~�@��@��@���@�@�Ĝ@�;d@��T@�@�V@�A�@ۅ@�^5@�V@�z�@���@׍P@��@��T@�I�@�K�@��@Ѳ-@ϝ�@��`@�5?@Ȭ@ȃ@��/@��`@�X@�Q�@ǍP@��`@���@ț�@�t�@Ł@���@�Ĝ@�Z@���@�~�@���@���@��!@�p�@�X@�G�@�/@��@�%@��@�9X@���@�33@���@�M�@�p�@��m@�"�@�33@�33@�@�v�@�;d@���@��m@��9@��@�J@���@�@��h@�p�@�X@�G�@�?}@�/@��@�V@���@��/@��9@�(�@��m@��
@�ƨ@���@�
=@��@�v�@�J@��#@��@�?}@�7L@���@�(�@��!@�{@��T@�@���@��h@���@�j@���@�S�@�C�@�33@�o@���@�ff@�M�@�-@��@�@��@���@�Ĝ@�z�@�(�@��m@��
@�"�@�~�@�M�@�{@��@�G�@��@�Ĝ@�j@��@��@��@��F@���@�|�@�+@�o@�
=@���@�{@�@�?}@���@��9@��9@���@���@�33@��y@��+@�-@���@���@�/@���@��`@��9@�r�@��P@�
=@�ȴ@���@�^5@�5?@��@�{@�M�@���@���@��@�^5@��#@��@��T@���@���@�V@���@�l�@�"�@�"�@��@�C�@�-@�n�@��H@�+@�
=@��y@��y@���@�~�@�~�@�~�@�-@�V@�v�@�n�@�V@�=q@�V@�E�@�-@���@���@��-@�hs@��`@�r�@�1'@��@�+@��@���@�=q@���@��-@��-@���@�O�@���@�V@�&�@�X@�hs@�/@��u@�Z@�9X@� �@���@��@��k@zE�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�By�By�By�By�By�By�By�By�By�By�Bz�By�Bz�Bz�By�By�By�By�B{�B~�B� B�B�DB��B�BK�Bs�B�B�DB�bB�uB�bB�DB�JB��B�^B�dB�jB��B�#B�
B�;B�B�B�BɺB��B��BŢB��B�jB�XB�LB�9B�-B�9B�FB�?B�-B�B��B��B��B�\B�1Bs�B^5BD�B33B!�B�BVB	7B%B��B�B�NBB
��B
�B
�;B
�qB
��B
�uB
�uB
x�B
S�B
E�B
A�B
8RB
)�B
�B
uB

=B	��B	�B	�#B	��B	�jB	�3B	�B	��B	��B	��B	��B	��B	�\B	|�B	m�B	dZB	VB	J�B	F�B	;dB	8RB	6FB	33B	-B	$�B	�B	bB��B�B��B�qB�qB�9B�jBɺB��B��B��B��BȴBŢB�wB�3B�B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�%B�B�B�B�B�B�B� B|�Bx�Bu�Bs�Br�Br�Br�Br�Bq�Bp�Bm�Bl�Bk�BiyBhsBgmBgmBiyBl�Bm�Bn�Bp�Bt�Bw�Bx�Bz�B~�B� B~�B}�By�By�Bx�Bx�Bv�Bs�BjBcTBaHBaHB`BB^5B`BBaHBaHBaHBbNBcTBdZBffBhsBp�Bw�B|�B� B�1B�7B�=B�VB�hB�{B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�oB�{B��B��B��B��B��B�B�'B�'B�3B�!B�?B�jB�qB�wB��B��B��BƨB��B�)B�/B�5B�;B�BB�HB�mB�yB�yB�yB�sB�sB�sB�sB�yB�B�B�B��B��B��B	<jB	?}B	D�B	F�B	G�B	I�B	K�B	L�B	N�B	N�B	N�B	O�B	P�B	Q�B	Q�B	Q�B	W
B	\)B	]/B	^5B	_;B	`BB	`BB	`BB	aHB	aHB	cTB	dZB	ffB	hsB	gmB	ffB	gmB	hsB	hsB	hsB	hsB	jB	l�B	u�B	x�B	y�B	z�B	{�B	~�B	�B	�+B	�1B	�=B	�DB	�DB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�9B	�FB	�XB	�XB	�dB	�jB	�wB	�}B	�}B	�}B	��B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�#B	�B	�BB	�BB	�BB	�BB	�/B	�HB	�NB	�ZB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
1B
DB
JB
\B
VB
\B
VB
VB
VB
\B
�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bz�By�By�By�By�By�By�By�By�By�By�Bz�By�Bz�Bz�By�By�By�By�B{�B~�B� B�B�DB��B�BK�Bs�B�B�DB�bB�uB�bB�DB�JB��B�^B�dB�jB��B�#B�
B�;B�B�B�BɺB��B��BŢB��B�jB�XB�LB�9B�-B�9B�FB�?B�-B�B��B��B��B�\B�1Bs�B^5BD�B33B!�B�BVB	7B%B��B�B�NBB
��B
�B
�;B
�qB
��B
�uB
�uB
x�B
S�B
E�B
A�B
8RB
)�B
�B
uB

=B	��B	�B	�#B	��B	�jB	�3B	�B	��B	��B	��B	��B	��B	�\B	|�B	m�B	dZB	VB	J�B	F�B	;dB	8RB	6FB	33B	-B	$�B	�B	bB��B�B��B�qB�qB�9B�jBɺB��B��B��B��BȴBŢB�wB�3B�B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�%B�B�B�B�B�B�B� B|�Bx�Bu�Bs�Br�Br�Br�Br�Bq�Bp�Bm�Bl�Bk�BiyBhsBgmBgmBiyBl�Bm�Bn�Bp�Bt�Bw�Bx�Bz�B~�B� B~�B}�By�By�Bx�Bx�Bv�Bs�BjBcTBaHBaHB`BB^5B`BBaHBaHBaHBbNBcTBdZBffBhsBp�Bw�B|�B� B�1B�7B�=B�VB�hB�{B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�oB�{B��B��B��B��B��B�B�'B�'B�3B�!B�?B�jB�qB�wB��B��B��BƨB��B�)B�/B�5B�;B�BB�HB�mB�yB�yB�yB�sB�sB�sB�sB�yB�B�B�B��B��B��B	<jB	?}B	D�B	F�B	G�B	I�B	K�B	L�B	N�B	N�B	N�B	O�B	P�B	Q�B	Q�B	Q�B	W
B	\)B	]/B	^5B	_;B	`BB	`BB	`BB	aHB	aHB	cTB	dZB	ffB	hsB	gmB	ffB	gmB	hsB	hsB	hsB	hsB	jB	l�B	u�B	x�B	y�B	z�B	{�B	~�B	�B	�+B	�1B	�=B	�DB	�DB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�9B	�FB	�XB	�XB	�dB	�jB	�wB	�}B	�}B	�}B	��B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�#B	�B	�BB	�BB	�BB	�BB	�/B	�HB	�NB	�ZB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
1B
DB
JB
\B
VB
\B
VB
VB
VB
\B
�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140845                              AO  ARCAADJP                                                                    20181024140845    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140845  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140845  QCF$                G�O�G�O�G�O�0               