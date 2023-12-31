CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:16Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190616  20181005190616  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ם��vU>1   @ם�$�R@3�5?|��c�ȴ9X1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�33@�  A��A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B/��B8  B@  BH  BP  BX  B_��Bh  BpffBx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B�33B�  B���B�  B�  B���C  C�fC�fC�fC
  C�C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C-�fC0  C2�C4  C6  C7�fC:  C<  C>�C@�CB�CD  CF  CH�CJ�CK�fCM�fCO�fCR  CT�CV  CW�fCY��C\  C^  C`  Cb�Cd33Cf33Ch�Cj  Ck�fCn�Cp  Cq�fCt33Cv33Cw�fCz  C|  C}�fC��C��fC��C��3C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C��C��3C�  C�  C��C��3C��C��3C�  C��C��C��C��C�  C��3C�  C��C�  C��3C��3C�  C��C��C��C�  C��C��C��C�  C��fC��3C�  C�  C��C�  C��3C��fC�  C��C��3C��fC��fC�  C��C��C�  C��fC�  C�  C��3C��fC�  C��C�  C��3C�  C��C��3C��3C�  C��3C�  C��C��C�  C�  C��C��3C��3C��3C��3C�  C��C�  C��fC�  C��C�  C��3C��C�  C��fD   D � D �3D� D  D� D  D� D  D� D  Ds3D�3Dy�D�3Ds3D�3Dy�D	  D	y�D	��D
� DfD�fD  D�fD  Dy�D  D� D  Dy�D��D� D  Dy�D�D� D  D�fDfD��D  Dy�D  D�fD  Dy�D  D��D  D� D  Dy�DfD� D��D�fD  D�fD  Ds3D  D�fD   D y�D!  D!�fD"  D"�fD"��D#� D$  D$� D%fD%�fD&  D&s3D'  D'��D(  D(s3D(��D)�fD*�D*� D*��D+� D,fD,� D,��D-� D.fD.�fD/fD/� D0  D0� D1  D1�fD2  D2y�D3  D3�fD4fD4� D4��D5s3D6  D6��D7�D7�fD8  D8� D8��D9y�D9��D:�fD;fD;��D<fD<� D<��D=� D>�D>� D>��D?y�D?�3D@y�DAfDA�fDB�DB�fDC  DC� DC��DD� DE�DE��DF�DF��DGfDG� DG��DH� DIfDI��DJ�DJ�fDK  DK��DL�DL��DM�DM��DNfDN�fDOfDO�fDPfDP��DQ�DQ��DR�DR��DSfDS��DTfDT�fDUfDU�fDV  DV� DW  DW�fDXfDX� DX��DYy�DZ  DZy�DZ��D[�fD\fD\� D\��D]� D^fD^�fD_  D_� D`  D`y�Da  Da�fDbfDby�Db��Dc�fDdfDd��DefDey�De�3Dfy�Df��Dg� Dh  Dh�fDifDi�fDjfDj�fDkfDk�fDk��Dly�Dm  Dm� DnfDn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr�fDs  Dsy�Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dw�fDy�
D�=�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@:=q@��@�Q�AA (�A@(�A`(�A�{A��HA�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B'��B/��B8
=B@
=BH
=BP
=BX
=B_��Bh
=Bpp�Bx
=B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�8RB�B�B�B�B�B�8RB�B�8RB�B���B�B�B���C�C��C��C��C
�C)C�C�C)C�C�C�C�C�C�C �C"�C$�C&�C'��C*�C,�C-��C0�C2)C4�C6�C7��C:�C<�C>)C@)CB)CD�CF�CH)CJ)CK��CM��CO��CR�CT)CV�CW��CY�\C\�C^�C`�Cb)Cd5�Cf5�Ch)Cj�Ck��Cn)Cp�Cq��Ct5�Cv5�Cw��Cz�C|�C}��C��C��C�C��{C�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�C�HC�C��{C�HC�HC�C��{C�C��{C�HC��C��C�C�C�HC��{C�HC�C�HC��{C��{C�HC�C��C��C�HC�C��C��C�HC��C��{C�HC�HC�C�HC��{C��C�HC�C��{C��C��C�HC��C��C�HC��C�HC�HC��{C��C�HC�C�HC��{C�HC�C��{C��{C�HC��{C�HC�C�C�HC�HC�C��{C��{C��{C��{C�HC�C�HC��C�HC�C�HC��{C�C�HC��D  �D ��D ��D��D �D��D �D��D �D��D �Ds�D��Dz>D��Ds�D��Dz>D	 �D	z>D	�>D
��D
D�
D �D�
D �Dz>D �D��D �Dz>D�>D��D �Dz>DqD��D �D�
D
D�qD �Dz>D �D�
D �Dz>D �D�qD �D��D �Dz>D
D��D�>D�
D �D�
D �Ds�D �D�
D  �D z>D! �D!�
D" �D"�
D"�>D#��D$ �D$��D%
D%�
D& �D&s�D' �D'�qD( �D(s�D(�>D)�
D*qD*��D*�>D+��D,
D,��D,�>D-��D.
D.�
D/
D/��D0 �D0��D1 �D1�
D2 �D2z>D3 �D3�
D4
D4��D4�>D5s�D6 �D6�qD7qD7�
D8 �D8��D8�>D9z>D9�>D:�
D;
D;�qD<
D<��D<�>D=��D>qD>��D>�>D?z>D?��D@z>DA
DA�
DBqDB�
DC �DC��DC�>DD��DEqDE�qDFqDF�qDG
DG��DG�>DH��DI
DI�qDJqDJ�
DK �DK�qDLqDL�qDMqDM�qDN
DN�
DO
DO�
DP
DP�qDQqDQ�qDRqDR�qDS
DS�qDT
DT�
DU
DU�
DV �DV��DW �DW�
DX
DX��DX�>DYz>DZ �DZz>DZ�>D[�
D\
D\��D\�>D]��D^
D^�
D_ �D_��D` �D`z>Da �Da�
Db
Dbz>Db�>Dc�
Dd
Dd�qDe
Dez>De��Dfz>Df�>Dg��Dh �Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dk�>Dlz>Dm �Dm��Dn
Dn��Do �Doz>Dp �Dp��Dq �Dq��Dr �Dr�
Ds �Dsz>Dt �Dt��Du �Du��Dv �Dvz>Dw �Dw��Dw�
Dy��D�>D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�AȁA�~�A�jA�t�A�v�A�`BA�jA�`BA�`BA�bNA�dZA�bNA�`BA�K�A�?}A��A�1A���A���A��A�  A�bA�A�A�1A�  A��;A��
A��
A���A���A���A��A��/A��A���A���A���A�AǴ9AǼjA���A�5?A��Aǩ�A�v�A�%A�A��A��HA��
AƇ+A�G�A�7LA��AōPA�?}A�(�Ağ�A�E�A��A��A���A�;dA��A�(�A��+A��mA���A��/A�bNA� �A��DA��wA���A�"�A��/A�%A��A��`A��A��/A��FA�z�A�9XA���A�9XA�hsA�n�A��A�Q�A���A�VA��A��uA�A�A�ȴA���A�1'A��PA���A�M�Ay�#AwO�Au�-At  ArQ�Apr�An�jAj��Ai�Ag��Afz�Ae�TAeS�AdȴAd-Aa�A\$�AY7LAX9XAUO�ATn�AR�AN�!AK`BAH  AG"�AE��AC�-A@I�A=7LA<(�A;XA:A�A7�A57LA3\)A2=qA0�DA.�`A-\)A,ĜA,�uA+"�A)�
A)K�A(�A'33A&5?A%VA#�^A!AbA�A��A�A\)AXAG�A;dA+A�A�RA��A	��A�+A�jA��A�/A;dAJA��A�A ��@�"�@�@�`B@�I�@���@��/@���@�|�@��`@�9X@�J@��y@�S�@�t�@�G�@�M�@��@�`B@߅@�dZ@׶F@׶F@�@���@�M�@�+@�5?@љ�@�@�@�K�@��@�|�@���@��@�dZ@�@��y@�J@�?}@̴9@�K�@�7L@ǝ�@�$�@�/@ě�@���@�dZ@�\)@öF@Å@��H@�J@�=q@�`B@��9@��m@�|�@���@�~�@���@�@��7@�V@���@�1@�ƨ@�|�@���@���@���@�v�@�5?@��T@�`B@���@�A�@���@�K�@�
=@���@��9@��P@�ȴ@�n�@��@��R@��@���@���@��@�S�@���@� �@���@�(�@�ȴ@�p�@�j@��m@��F@�|�@�\)@�l�@�l�@���@���@�X@�&�@�p�@�p�@�Ĝ@���@��@��@��@�l�@�
=@�ȴ@���@�v�@�=q@�5?@�M�@�M�@��@���@��#@���@��-@���@�x�@�7L@��@���@��9@��@�bN@�A�@�1@���@�t�@��R@�{@�?}@��@���@���@��`@���@��9@�j@�(�@���@�t�@�o@��y@���@��R@���@���@��\@��@��@���@�j@�Z@�A�@�b@��m@���@�ƨ@��w@��F@�t�@�C�@�+@�o@�
=@�o@��H@���@�v�@�V@���@���@�hs@���@�Z@�9X@�1@��F@�\)@���@��@���@��\@���@���@�~�@�5?@��@�@��^@���@�hs@��@���@��9@���@�Q�@��;@��
@���@�dZ@���@�5?@��T@��7@�/@�%@��/@���@��u@�bN@���@���@�33@��y@�ȴ@��!@��+@�v�@�ff@�$�@�{@�{@��#@��^@���@���@��@�&�@��9@��@�r�@�Z@�b@��@�\)@�+@��@��y@���@���@�ff@�{@���@���@�x�@�/@���@��u@�r�@�9X@�ƨ@�dZ@��@�@�ȴ@�V@�@��@��@���@�@�@���@�hs@�V@�Ĝ@�I�@�1'@���@�l�@�33@�
=@�@�@��@�v�@�5?@�$�@�J@��#@��@�r�@n��@\Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�~�AȁA�~�A�jA�t�A�v�A�`BA�jA�`BA�`BA�bNA�dZA�bNA�`BA�K�A�?}A��A�1A���A���A��A�  A�bA�A�A�1A�  A��;A��
A��
A���A���A���A��A��/A��A���A���A���A�AǴ9AǼjA���A�5?A��Aǩ�A�v�A�%A�A��A��HA��
AƇ+A�G�A�7LA��AōPA�?}A�(�Ağ�A�E�A��A��A���A�;dA��A�(�A��+A��mA���A��/A�bNA� �A��DA��wA���A�"�A��/A�%A��A��`A��A��/A��FA�z�A�9XA���A�9XA�hsA�n�A��A�Q�A���A�VA��A��uA�A�A�ȴA���A�1'A��PA���A�M�Ay�#AwO�Au�-At  ArQ�Apr�An�jAj��Ai�Ag��Afz�Ae�TAeS�AdȴAd-Aa�A\$�AY7LAX9XAUO�ATn�AR�AN�!AK`BAH  AG"�AE��AC�-A@I�A=7LA<(�A;XA:A�A7�A57LA3\)A2=qA0�DA.�`A-\)A,ĜA,�uA+"�A)�
A)K�A(�A'33A&5?A%VA#�^A!AbA�A��A�A\)AXAG�A;dA+A�A�RA��A	��A�+A�jA��A�/A;dAJA��A�A ��@�"�@�@�`B@�I�@���@��/@���@�|�@��`@�9X@�J@��y@�S�@�t�@�G�@�M�@��@�`B@߅@�dZ@׶F@׶F@�@���@�M�@�+@�5?@љ�@�@�@�K�@��@�|�@���@��@�dZ@�@��y@�J@�?}@̴9@�K�@�7L@ǝ�@�$�@�/@ě�@���@�dZ@�\)@öF@Å@��H@�J@�=q@�`B@��9@��m@�|�@���@�~�@���@�@��7@�V@���@�1@�ƨ@�|�@���@���@���@�v�@�5?@��T@�`B@���@�A�@���@�K�@�
=@���@��9@��P@�ȴ@�n�@��@��R@��@���@���@��@�S�@���@� �@���@�(�@�ȴ@�p�@�j@��m@��F@�|�@�\)@�l�@�l�@���@���@�X@�&�@�p�@�p�@�Ĝ@���@��@��@��@�l�@�
=@�ȴ@���@�v�@�=q@�5?@�M�@�M�@��@���@��#@���@��-@���@�x�@�7L@��@���@��9@��@�bN@�A�@�1@���@�t�@��R@�{@�?}@��@���@���@��`@���@��9@�j@�(�@���@�t�@�o@��y@���@��R@���@���@��\@��@��@���@�j@�Z@�A�@�b@��m@���@�ƨ@��w@��F@�t�@�C�@�+@�o@�
=@�o@��H@���@�v�@�V@���@���@�hs@���@�Z@�9X@�1@��F@�\)@���@��@���@��\@���@���@�~�@�5?@��@�@��^@���@�hs@��@���@��9@���@�Q�@��;@��
@���@�dZ@���@�5?@��T@��7@�/@�%@��/@���@��u@�bN@���@���@�33@��y@�ȴ@��!@��+@�v�@�ff@�$�@�{@�{@��#@��^@���@���@��@�&�@��9@��@�r�@�Z@�b@��@�\)@�+@��@��y@���@���@�ff@�{@���@���@�x�@�/@���@��u@�r�@�9X@�ƨ@�dZ@��@�@�ȴ@�V@�@��@��@���@�@�@���@�hs@�V@�Ĝ@�I�@�1'@���@�l�@�33@�
=@�@�@��@�v�@�5?@�$�@�J@��#@��@�r�@n��@\Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
~�B
}�B
~�B
~�B
�B
�DB
�\B
�VB
�\B
�oB
�oB
�uB
��B
��B
��B
��B
��B
�B
�'B
�?B
�^B
��B
ɺB
��B
�#B
�BB
��B�BW
Bo�B�B�B�B�3B�LB�XB��B�ZB�B��BbB�B#�B33BE�BVBVBS�BQ�BH�BD�B>wB.B �B�BuBbBB�yBƨB�LB��B~�B�FB�}B�B��Bs�B^5BH�B;dB49B'�B�BBB
��B
�B
�5B
��B
ƨB
�jB
�%B
ZB
O�B
A�B
)�B	�B	�5B	�B	��B	��B	��B	��B	�-B	��B	�bB	�B	}�B	v�B	o�B	hsB	W
B	8RB	)�B	!�B	oB	PB	+B��B�B�BB�)B��BǮB�XB�3B�'B�B��B��B��B��B��B�VB�%B�B�B�B� B~�B}�Bz�Bv�Bs�Bp�Bo�Bx�Bo�Bs�Bv�B�%B�%B�%B�%B�%B�%B�B�B�B�%B�7B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�9BBǮBȴB��BɺB��BB��B��B��B�RB�dB��B�)B�B��BBBƨB��B�B�BB�fB�`B�`B�TB�ZB�fB�mB�mB�`B�fB�NB�5B�/B�5B�;B�HB�`B�B�B��B��B��B	  B	B	  B��B	B	B��B	  B	1B	DB	VB	oB	�B	�B	�B	�B	 �B	!�B	"�B	%�B	(�B	,B	/B	2-B	7LB	9XB	:^B	9XB	5?B	33B	6FB	9XB	:^B	A�B	E�B	F�B	D�B	G�B	J�B	L�B	R�B	W
B	W
B	T�B	W
B	ZB	[#B	\)B	^5B	_;B	`BB	`BB	aHB	bNB	cTB	ffB	l�B	l�B	k�B	iyB	l�B	p�B	v�B	x�B	z�B	{�B	|�B	}�B	�B	�+B	�7B	�JB	�PB	�PB	�PB	�PB	�PB	�VB	�VB	�\B	�hB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�?B	�?B	�FB	�LB	�LB	�RB	�RB	�^B	�qB	�wB	�}B	��B	B	ĜB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
+B
+B
%B
%B
+B
+B
+B
1B
1B
1B
1B

=B

=B

=B

=B

=B
JB
�B
�B
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
~�B
}�B
~�B
~�B
�B
�DB
�\B
�VB
�\B
�oB
�oB
�uB
��B
��B
��B
��B
��B
�B
�'B
�?B
�^B
��B
ɺB
��B
�#B
�BB
��B�BW
Bo�B�B�B�B�3B�LB�XB��B�ZB�B��BbB�B#�B33BE�BVBVBS�BQ�BH�BD�B>wB.B �B�BuBbBB�yBƨB�LB��B~�B�FB�}B�B��Bs�B^5BH�B;dB49B'�B�BBB
��B
�B
�5B
��B
ƨB
�jB
�%B
ZB
O�B
A�B
)�B	�B	�5B	�B	��B	��B	��B	��B	�-B	��B	�bB	�B	}�B	v�B	o�B	hsB	W
B	8RB	)�B	!�B	oB	PB	+B��B�B�BB�)B��BǮB�XB�3B�'B�B��B��B��B��B��B�VB�%B�B�B�B� B~�B}�Bz�Bv�Bs�Bp�Bo�Bx�Bo�Bs�Bv�B�%B�%B�%B�%B�%B�%B�B�B�B�%B�7B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�9BBǮBȴB��BɺB��BB��B��B��B�RB�dB��B�)B�B��BBBƨB��B�B�BB�fB�`B�`B�TB�ZB�fB�mB�mB�`B�fB�NB�5B�/B�5B�;B�HB�`B�B�B��B��B��B	  B	B	  B��B	B	B��B	  B	1B	DB	VB	oB	�B	�B	�B	�B	 �B	!�B	"�B	%�B	(�B	,B	/B	2-B	7LB	9XB	:^B	9XB	5?B	33B	6FB	9XB	:^B	A�B	E�B	F�B	D�B	G�B	J�B	L�B	R�B	W
B	W
B	T�B	W
B	ZB	[#B	\)B	^5B	_;B	`BB	`BB	aHB	bNB	cTB	ffB	l�B	l�B	k�B	iyB	l�B	p�B	v�B	x�B	z�B	{�B	|�B	}�B	�B	�+B	�7B	�JB	�PB	�PB	�PB	�PB	�PB	�VB	�VB	�\B	�hB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�?B	�?B	�FB	�LB	�LB	�RB	�RB	�^B	�qB	�wB	�}B	��B	B	ĜB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
+B
+B
%B
%B
+B
+B
+B
1B
1B
1B
1B

=B

=B

=B

=B

=B
JB
�B
�B
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190616                              AO  ARCAADJP                                                                    20181005190616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190616  QCF$                G�O�G�O�G�O�8000            