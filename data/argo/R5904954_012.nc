CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:50Z creation      
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
resolution        =���   axis      Z        t  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  dT   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  f4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191650  20181005191650  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ץƼ�1   @ץ�F)�b@3�"��`B�c�n��P1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�33B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CE�fCG�fCJ  CL  CM�fCO�fCQ�fCS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv�Cx�Cz�C|  C~  C�  C��3C��3C��3C�  C�  C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C��3C��3C��3C�  C��C��C��C��C��C�  C��3C��C��3C��3C��C��C��3C��3C��3C��C��C��3C�  C��3C��C��3C��C�  C��3C�  C��C�  C��3C��C�  C��C�  C�  C�  C�  C��C��3C�  C��3C�  C��3C�  C��C�  C�  C��C��3C��fC��C��C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C��3C�  C��C�  C�  D   D � DfD�fD  Ds3D�3Dy�DfD� D�3Dy�D  D� D  D�fD  Dy�D��D	y�D	��D
y�D
��D�fD  D� DfD� D  Dy�D  D�fD  D� D  Dy�D��D� D  Dy�D��D�fDfD� D��D� D  D�fDfD� D��Dy�D��Ds3D��Dy�D��D� D  D�fDfD�fDfD�fD fD � D!  D!� D"fD"y�D#  D#� D#��D$y�D$��D%� D&fD&y�D&�3D'y�D(  D(�fD)  D)� D*  D*� D*��D+y�D,  D,�fD-  D-�fD-��D.y�D.��D/� D0  D0� D1  D1y�D2  D2�fD3fD3� D4  D4�fD5  D5y�D6  D6�fD7  D7y�D8fD8y�D9fD9� D9�3D:�fD;  D;�fD<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DFfDF�fDF��DGy�DHfDH��DI  DI� DJfDJ� DK  DK� DK��DLy�DM  DM�fDNfDN�fDO  DO�fDPfDPy�DQ  DQ� DR  DR� DS  DSy�DS��DTy�DU  DUy�DU��DVy�DV��DWy�DX  DX�fDX��DYy�DZfDZy�D[fD[��D\  D\y�D]fD]�fD]�3D^� D_�D_�fD`  D`s3D`��Da� DbfDb� Dc  Dc� Dd  Dd�fDefDe� De��Dfs3Df��Dgy�Dg��Dh� Di  Di� Dj  Djy�Dk  Dk�fDlfDl�fDm  Dmy�Dm��Dn� DofDo��Dp�Dp�fDqfDq� Dq��Dr� DsfDs�fDtfDt�fDu  Du� Du��Dv� DwfDw� Dw�3Dy�HD�S3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B��B�Q�B�Q�B��B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B؅B�Q�B�Q�B��B�Q�B�B�Q�B��B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"\C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<\C>(�C@(�CB(�CD(�CF\CH\CJ(�CL(�CN\CP\CR\CT\CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn\Cp(�Cr(�Ct(�CvB�CxB�CzB�C|(�C~(�C�{C��C��C��C�{C�{C��C�{C�!HC�{C��C��C��C�{C�{C�{C�{C�{C��C�{C�{C�!HC�{C�{C�{C�{C�!HC�{C��C�{C�!HC�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C��C��C��C��C��C��C�{C�!HC�{C�{C�{C��C��C��C�{C�!HC�!HC�!HC�!HC�!HC�{C��C�!HC��C��C�!HC�!HC��C��C��C�!HC�!HC��C�{C��C�!HC��C�!HC�{C��C�{C�!HC�{C��C�!HC�{C�!HC�{C�{C�{C�{C�!HC��C�{C��C�{C��C�{C�!HC�{C�{C�!HC��C���C�!HC�!HC��C�{C��C�{C�{C�{C�{C�{C�{C�{C�!HC��C��C��C�{C�!HC�{C�{D 
=D �=D�D��D
=D}pD�pD��D�D�=D�pD��D
=D�=D
=D��D
=D��D	�D	��D
�D
��D�D��D
=D�=D�D�=D
=D��D
=D��D
=D�=D
=D��D�D�=D
=D��D�D��D�D�=D�D�=D
=D��D�D�=D�D��D�D}pD�D��D�D�=D
=D��D�D��D�D��D �D �=D!
=D!�=D"�D"��D#
=D#�=D$�D$��D%�D%�=D&�D&��D&�pD'��D(
=D(��D)
=D)�=D*
=D*�=D+�D+��D,
=D,��D-
=D-��D.�D.��D/�D/�=D0
=D0�=D1
=D1��D2
=D2��D3�D3�=D4
=D4��D5
=D5��D6
=D6��D7
=D7��D8�D8��D9�D9�=D9�pD:��D;
=D;��D<
=D<�=D=
=D=�=D>�D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC
=DC��DD
=DD�=DE
=DE�=DF�DF��DG�DG��DH�DH�
DI
=DI�=DJ�DJ�=DK
=DK�=DL�DL��DM
=DM��DN�DN��DO
=DO��DP�DP��DQ
=DQ�=DR
=DR�=DS
=DS��DT�DT��DU
=DU��DV�DV��DW�DW��DX
=DX��DY�DY��DZ�DZ��D[�D[�
D\
=D\��D]�D]��D]�pD^�=D_
D_��D`
=D`}pDa�Da�=Db�Db�=Dc
=Dc�=Dd
=Dd��De�De�=Df�Df}pDg�Dg��Dh�Dh�=Di
=Di�=Dj
=Dj��Dk
=Dk��Dl�Dl��Dm
=Dm��Dn�Dn�=Do�Do�
Dp
Dp��Dq�Dq�=Dr�Dr�=Ds�Ds��Dt�Dt��Du
=Du�=Dv�Dv�=Dw�Dw�=Dw�pDy��D�XR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��mA��yA��yA��mA��mA��HA��#A��
A���A���A�ȴAͩ�A͓uA�r�A�VA��A��/A̓uA�p�A�VA��A���A��/A˛�A�G�A���A�ȴA�v�A�/A� �AȺ^A�bA���Aǡ�A�$�A��AƗ�A�9XA�ȴA�JAĮA�&�A�`BA�33A�A�hsA�
=A�%A�=qA��A��jA���A�C�A�|�A�ȴA��PA�-A��TA�|�A� �A��mA���A�JA��;A�n�A�p�A�ȴA���A���A�n�A�\)A���A�v�A�-A�?}A���A�  A��A�t�A��RA���A��A�33A�{A�ȴA�ĜA��A��+A��hA�JA��HA�hsA���A��PA��`A�=qA~�A|�+Az�Av�At5?Ao��Al1'Aj��AiK�Af��Ad�Acx�A`ZA]G�A\5?AZ�!AXȴAV�DAU/ATjAT�AR�AL��AIXAF�9AA��A@�A>��A=S�A<A;�A:ZA8�!A5��A2�RA1�A0�\A0  A/|�A/�A.�uA-�A,��A+\)A*I�A)K�A(�A&�+A$�/A$9XA#�TA#�hA!�wA!�A n�A��Ax�AVA�;AS�A�
AA5?A�7A��A{AdZA�DA�mA�RA��Az�AZAt�A"�AVA�DA�hA+A�+A5?A�^A	�#A	oA�9A��A^5AA�A�A�
A�Ap�AA�!AƨAK�A�A�uAn�A$�AAC�A jA @�"�@���@��#@�V@�@��/@��@�u@�j@��;@�K�@��y@��@�z�@�@�t�@�@��@�j@���@�o@◍@�7@߶F@�p�@܃@���@��@ڸR@ם�@�^5@�/@�z�@�^5@мj@ЋD@��
@��@�7L@ˍP@���@ə�@Ȭ@�S�@�=q@ź^@�G�@�z�@î@�"�@°!@+@�{@���@�I�@��
@�\)@���@�V@�p�@�G�@�V@���@�bN@��@��F@�l�@���@�~�@�$�@��T@�V@���@�Z@� �@��@�V@��T@��@�hs@�%@���@�K�@���@���@�K�@��+@��h@�/@���@��D@�Q�@�9X@��@��@���@���@�ȴ@�J@�O�@��`@�z�@���@�dZ@��H@�E�@�5?@���@�`B@�(�@��
@��
@��;@��@��@���@�@���@�O�@�V@��@�b@�1'@�9X@�b@�A�@��
@�33@��@�p�@�hs@�X@�G�@�&�@��`@�I�@�;d@��@�
=@��H@���@��!@��\@�M�@��@��^@���@�5?@�J@���@�$�@�ff@��-@��@��/@���@��@�bN@�I�@� �@��
@���@�;d@�"�@�
=@��y@���@�=q@��T@�?}@�I�@��P@�;d@��@��@���@���@���@�5?@��@�?}@��@���@�j@��m@���@�|�@�dZ@�"�@��@��@�ȴ@���@���@��R@��!@���@�^5@�=q@��@���@��@�bN@�r�@�bN@�l�@�"�@��@���@��+@�n�@�V@�M�@�=q@�5?@�E�@�-@��@���@�X@���@�1'@���@��@���@��@�"�@��@�n�@�@��@�@��h@�`B@�&�@���@�Ĝ@�j@�  @���@��@�S�@�o@��@��@��@��@���@���@�^5@�=q@��@��T@���@��@�`B@�/@�V@�Ĝ@��@�I�@�A�@� �@�b@�  @��w@��w@���@��@�dZ@�K�@�33@�+@�o@�@�_@q��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��yA��yA��mA��mA��HA��#A��
A���A���A�ȴAͩ�A͓uA�r�A�VA��A��/A̓uA�p�A�VA��A���A��/A˛�A�G�A���A�ȴA�v�A�/A� �AȺ^A�bA���Aǡ�A�$�A��AƗ�A�9XA�ȴA�JAĮA�&�A�`BA�33A�A�hsA�
=A�%A�=qA��A��jA���A�C�A�|�A�ȴA��PA�-A��TA�|�A� �A��mA���A�JA��;A�n�A�p�A�ȴA���A���A�n�A�\)A���A�v�A�-A�?}A���A�  A��A�t�A��RA���A��A�33A�{A�ȴA�ĜA��A��+A��hA�JA��HA�hsA���A��PA��`A�=qA~�A|�+Az�Av�At5?Ao��Al1'Aj��AiK�Af��Ad�Acx�A`ZA]G�A\5?AZ�!AXȴAV�DAU/ATjAT�AR�AL��AIXAF�9AA��A@�A>��A=S�A<A;�A:ZA8�!A5��A2�RA1�A0�\A0  A/|�A/�A.�uA-�A,��A+\)A*I�A)K�A(�A&�+A$�/A$9XA#�TA#�hA!�wA!�A n�A��Ax�AVA�;AS�A�
AA5?A�7A��A{AdZA�DA�mA�RA��Az�AZAt�A"�AVA�DA�hA+A�+A5?A�^A	�#A	oA�9A��A^5AA�A�A�
A�Ap�AA�!AƨAK�A�A�uAn�A$�AAC�A jA @�"�@���@��#@�V@�@��/@��@�u@�j@��;@�K�@��y@��@�z�@�@�t�@�@��@�j@���@�o@◍@�7@߶F@�p�@܃@���@��@ڸR@ם�@�^5@�/@�z�@�^5@мj@ЋD@��
@��@�7L@ˍP@���@ə�@Ȭ@�S�@�=q@ź^@�G�@�z�@î@�"�@°!@+@�{@���@�I�@��
@�\)@���@�V@�p�@�G�@�V@���@�bN@��@��F@�l�@���@�~�@�$�@��T@�V@���@�Z@� �@��@�V@��T@��@�hs@�%@���@�K�@���@���@�K�@��+@��h@�/@���@��D@�Q�@�9X@��@��@���@���@�ȴ@�J@�O�@��`@�z�@���@�dZ@��H@�E�@�5?@���@�`B@�(�@��
@��
@��;@��@��@���@�@���@�O�@�V@��@�b@�1'@�9X@�b@�A�@��
@�33@��@�p�@�hs@�X@�G�@�&�@��`@�I�@�;d@��@�
=@��H@���@��!@��\@�M�@��@��^@���@�5?@�J@���@�$�@�ff@��-@��@��/@���@��@�bN@�I�@� �@��
@���@�;d@�"�@�
=@��y@���@�=q@��T@�?}@�I�@��P@�;d@��@��@���@���@���@�5?@��@�?}@��@���@�j@��m@���@�|�@�dZ@�"�@��@��@�ȴ@���@���@��R@��!@���@�^5@�=q@��@���@��@�bN@�r�@�bN@�l�@�"�@��@���@��+@�n�@�V@�M�@�=q@�5?@�E�@�-@��@���@�X@���@�1'@���@��@���@��@�"�@��@�n�@�@��@�@��h@�`B@�&�@���@�Ĝ@�j@�  @���@��@�S�@�o@��@��@��@��@���@���@�^5@�=q@��@��T@���@��@�`B@�/@�V@�Ĝ@��@�I�@�A�@� �@�b@�  @��w@��w@���@��@�dZ@�K�@�33@�+@�o@�@�_@q��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
w�B
x�B
y�B
z�B
|�B
�B
�%B
�1B
�DB
�VB
�oB
��B
�'B
ŢB
�
B
�BB\BoB�B�B%�B)�B1'B9XB@�BM�BK�BL�BYBdZBr�B{�Bx�B� B�B~�Bv�Bw�B�=B��B�FB�jB��B	7B�B�B,B,B0!B/B.B0!B1'BB�BL�BL�BN�BP�BR�BS�BXBYBT�BN�BF�B<jB1'B�B1B��B�`B�B�B��B�}B�!B�JBVB;dB-B%�B!�B�B�BPB
��B
�B
�5B
��B
��B
�B
u�B
ZB
D�B
:^B
,B
�B
	7B	��B	�HB	ĜB	�B	��B	��B	�B	u�B	l�B	]/B	K�B	C�B	;dB	0!B	#�B	�B	�B	oB	+B�B�;B��B��BŢB�wB�^B�FB�9B�B��B��B��B�!B�!B�B�B�B�B��B��B��B��B��B�{B��B��B��B��B��B��B�uB�oB�hB�\B�\B�PB�JB�JB�DB�=B�1B�+B�B�1B�+B�+B�7B�7B�7B�1B�=B�DB�DB�PB�PB�VB�VB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�?B�FB�dB�jB�qB�qB�wB�wB�wB�}B�wB�dB�XB�LB�RB�jB�}B�dB�LB�XB�dB�^B�^B�}B��B��BBǮB��B��B��B��B��B�B�NB�TB�mB�B�B�B�B��B��B��B��B��B��B	B	B	B	B	B	+B	PB	VB	\B	bB	oB	�B	�B	�B	�B	�B	 �B	"�B	(�B	,B	-B	.B	0!B	2-B	49B	5?B	6FB	6FB	7LB	;dB	?}B	C�B	H�B	H�B	E�B	E�B	E�B	F�B	H�B	H�B	K�B	L�B	N�B	P�B	P�B	S�B	XB	YB	ZB	^5B	`BB	e`B	hsB	hsB	jB	m�B	q�B	r�B	s�B	t�B	y�B	}�B	�B	�%B	�+B	�1B	�1B	�=B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�?B	�?B	�FB	�RB	�dB	�XB	�LB	�XB	�jB	�qB	��B	ÖB	ĜB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
#�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
w�B
x�B
y�B
z�B
|�B
�B
�%B
�1B
�DB
�VB
�oB
��B
�'B
ŢB
�
B
�BB\BoB�B�B%�B)�B1'B9XB@�BM�BK�BL�BYBdZBr�B{�Bx�B� B�B~�Bv�Bw�B�=B��B�FB�jB��B	7B�B�B,B,B0!B/B.B0!B1'BB�BL�BL�BN�BP�BR�BS�BXBYBT�BN�BF�B<jB1'B�B1B��B�`B�B�B��B�}B�!B�JBVB;dB-B%�B!�B�B�BPB
��B
�B
�5B
��B
��B
�B
u�B
ZB
D�B
:^B
,B
�B
	7B	��B	�HB	ĜB	�B	��B	��B	�B	u�B	l�B	]/B	K�B	C�B	;dB	0!B	#�B	�B	�B	oB	+B�B�;B��B��BŢB�wB�^B�FB�9B�B��B��B��B�!B�!B�B�B�B�B��B��B��B��B��B�{B��B��B��B��B��B��B�uB�oB�hB�\B�\B�PB�JB�JB�DB�=B�1B�+B�B�1B�+B�+B�7B�7B�7B�1B�=B�DB�DB�PB�PB�VB�VB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�?B�FB�dB�jB�qB�qB�wB�wB�wB�}B�wB�dB�XB�LB�RB�jB�}B�dB�LB�XB�dB�^B�^B�}B��B��BBǮB��B��B��B��B��B�B�NB�TB�mB�B�B�B�B��B��B��B��B��B��B	B	B	B	B	B	+B	PB	VB	\B	bB	oB	�B	�B	�B	�B	�B	 �B	"�B	(�B	,B	-B	.B	0!B	2-B	49B	5?B	6FB	6FB	7LB	;dB	?}B	C�B	H�B	H�B	E�B	E�B	E�B	F�B	H�B	H�B	K�B	L�B	N�B	P�B	P�B	S�B	XB	YB	ZB	^5B	`BB	e`B	hsB	hsB	jB	m�B	q�B	r�B	s�B	t�B	y�B	}�B	�B	�%B	�+B	�1B	�1B	�=B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�?B	�?B	�FB	�RB	�dB	�XB	�LB	�XB	�jB	�qB	��B	ÖB	ĜB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
#�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191650                              AO  ARCAADJP                                                                    20181005191650    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191650  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191650  QCF$                G�O�G�O�G�O�8000            