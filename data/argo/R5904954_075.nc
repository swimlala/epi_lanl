CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:06Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191706  20181005191706  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               KA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$���1   @��%WM@5&$�/��d#��-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      KA   A   A   @9��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B�  B���B�  B�33B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C�C�C  C  C   C!�fC$  C&  C(  C*  C,  C-�fC0  C2�C4�C6  C7�fC9�fC<  C>  C@  CB  CC�fCE�fCH  CJ  CL�CN�CO�fCR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx�Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��C�  C�  C�  C�  C�  C��C��C�  C��3C��C��C�  C��C�  C��C�  C�  C�  C�  C��C�  C��C��3C��3C��3C��3C�  C�  C��3C�  C��3C��3C��C��3C�  C��C��C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C��3C��3C��3C��3C�  C��C��C�  C�  C��3C�  C��3C��fC��3C��C��C��C��3C��3C�  C��C��C�  C�  C��C��C��C�  C��C��C��C��C��C��C��C��C��C�  C��3C�  C�  C��3C�  C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C��C��C��C��C�  C�  D fD � D ��Dy�D��Dy�D��Dy�D��Dy�D  D�fDfD�fD  D� D��D� D	fD	� D	��D
y�D
��D� DfD�fD��Dy�D��Dy�D��Dy�D��D� DfD� D��Dy�DfD� DfD� D  D� D  Dy�D�3Dy�D��D$�D$�fD%  D%y�D&  D&� D'fD'�fD(  D(� D)  D)�fD*fD*y�D+  D+�fD,  D,� D-  D-�fD.  D.� D/fD/�fD0fD0y�D1  D1�fD2  D2y�D3  D3� D3��D4� D4��D5y�D6fD6� D7  D7�fD8  D8y�D8�3D9� D:fD:� D;  D;�fD<fD<� D=fD=�fD>  D>y�D>��D?� D@fD@� D@��DA� DA��DB� DCfDC�fDD  DDy�DE  DE� DF  DF� DG  DG�fDH  DH� DH��DI� DJfDJ�fDKfDK� DK��DLy�DL��DM� DNfDN�fDO  DO� DP  DP� DP��DQy�DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV�fDWfDW� DX  DX� DX��DY� DZfDZ�fD[  D[y�D\  D\�fD]fD]�fD^fD^� D_  D_y�D`  D`�fDa�Da�fDb  Db� DcfDc� Dd  Dd� DefDe�fDf  Df� Dg  Dgy�Dh  Dh�fDifDi�fDjfDj� Dk  Dk� Dl  Dl�fDmfDm�fDn  Dn� DofDo� Dp  Dp� DqfDqy�Dr  Dr�fDs  Ds�fDtfDt�fDufDu�fDvfDv�fDwfDw� Dw��Dy�fD�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Ffg@�ff@�ffA33A#33AC33Ac33A�fgA���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B)33B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B�ffB�ffB�33B�ffB䙙B�ffB�33B�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33CL�C33C33C33C33CL�CL�C33C33C 33C"�C$33C&33C(33C*33C,33C.�C033C2L�C4L�C633C8�C:�C<33C>33C@33CB33CD�CF�CH33CJ33CLL�CNL�CP�CR33CT33CV33CX33CZ33C\33C^�C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33CtL�CvL�CxL�Cz33C|33C~�C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C�&gC��C��C��C��C��C�&gC�&gC��C��C�&gC�&gC��C�&gC��C�&gC��C��C��C��C�&gC��C�&gC��C��C��C��C��C��C��C��C��C��C�&gC��C��C�&gC�&gC��C��C��C��C�&gC��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C�  C��C�&gC�34C�&gC��C��C��C�&gC�&gC��C��C�&gC�&gC�&gC��C�&gC�&gC�&gC�&gC�34C�34C�&gC�&gC�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C�&gC�&gC�&gC�&gC��C��D 3D ��DgD�gDgD�gDgD�gDgD�gD�D�3D3D�3D�D��DgD��D	3D	��D
gD
�gDgD��D3D�3DgD�gDgD�gDgD�gDgD��D3D��DgD�gD3D��D3D��D�D��D�D�gD  D�gDgD$�D$�3D%�D%�gD&�D&��D'3D'�3D(�D(��D)�D)�3D*3D*�gD+�D+�3D,�D,��D-�D-�3D.�D.��D/3D/�3D03D0�gD1�D1�3D2�D2�gD3�D3��D4gD4��D5gD5�gD63D6��D7�D7�3D8�D8�gD9  D9��D:3D:��D;�D;�3D<3D<��D=3D=�3D>�D>�gD?gD?��D@3D@��DAgDA��DBgDB��DC3DC�3DD�DD�gDE�DE��DF�DF��DG�DG�3DH�DH��DIgDI��DJ3DJ�3DK3DK��DLgDL�gDMgDM��DN3DN�3DO�DO��DP�DP��DQgDQ�gDR�DR��DS�DS�3DT�DT��DU�DU��DV�DV�3DW3DW��DX�DX��DYgDY��DZ3DZ�3D[�D[�gD\�D\�3D]3D]�3D^3D^��D_�D_�gD`�D`�3Da�Da�3Db�Db��Dc3Dc��Dd�Dd��De3De�3Df�Df��Dg�Dg�gDh�Dh�3Di3Di�3Dj3Dj��Dk�Dk��Dl�Dl�3Dm3Dm�3Dn�Dn��Do3Do��Dp�Dp��Dq3Dq�gDr�Dr�3Ds�Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw��DwٚDy�3D�>f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A܏\A܍PAܙ�Aܙ�Aܛ�Aܛ�Aܝ�Aܝ�Aܟ�Aܡ�Aܥ�Aܥ�A�$�Aۗ�A�VA�$�A�A�ĜAڗ�A�{A�M�A��A�/A�O�AёhAЩ�AσA�S�A�bNA��A�XAť�A�JAÑhA�I�A���A��9A�JA�"�A� �A�VA�M�A��PA��FA�"�A�A��A���A�ĜA��A��TA�$�A��wA�C�A�
=A�A�{A��A��/A�%A�n�A��/A�n�A�%A��A�|�A��TA�/A�~�A�I�A���A���A�hsA�9XA�?}A�33A�C�A��`A���A�%A���A�t�A��wA�G�A���A�t�A��uA���A���A�t�A�z�A���A�K�A��-A�$�A�ȴA��mA��/A���A��A��uA�l�A��HA��PA�A�A���A��#A��9A�E�A���Azz�Av��Au��As�FAp�jAm/Af��Ad��Aa��AZĜAUx�AS
=AP^5AMƨAJA�AGt�AFQ�AC�^AB��A@��A>��A=��A;��A9��A6�HA5A4��A3��A2��A0�yA.��A.�A-�
A,�9A*ĜA)O�A(v�A'l�A&�HA&��A$�/A!�hA!33A ��A�A   A��AȴA�+A{A�hA�mA�`A��A��A�!A��AXA�An�A�#A�wA�-Av�AXA?}A��A5?A�TAl�A?}A"�A�A�A
��A�9A`BA��A�DA=qA%A��A`BAr�AM�At�A�AVA�+A�HAJAO�A�jAoA�uA;d@�l�@��9@��F@���@��`@�+@�X@�ȴ@��T@���@�"�@�P@���@���@�l�@���@�33@�S�@�F@�b@�j@�bN@�ȴ@�@�X@��@ܴ9@ۮ@�"�@ڸR@�M�@��@���@֗�@��@�A�@�^5@�$�@ѡ�@��@�X@�ȴ@�v�@��m@�K�@�hs@ȃ@�r�@�Z@��;@�
=@�b@�$�@��@��@�1@�1@���@���@�K�@�~�@�`B@���@��D@��F@�@��y@���@��@��-@�K�@��@�@��h@�/@�V@�bN@��@��w@��R@�=q@���@���@��w@��y@��!@�v�@���@��@�%@�b@�Q�@��;@��u@���@�O�@��#@�v�@���@���@��@�7L@� �@�l�@�^5@���@�M�@�l�@�@���@���@�
=@�S�@�dZ@�dZ@�
=@���@��@�C�@���@��F@��
@��@��@�;d@��R@��@��#@��@��7@�p�@���@��;@���@�C�@���@�~�@�n�@�~�@�V@�$�@�@�V@�Q�@�ƨ@�I�@�j@���@��j@���@�b@�\)@��w@�l�@�-@�@���@�`B@�x�@���@��@�J@��@��h@��@�1'@�@��y@��y@���@�o@���@�n�@��\@��+@�M�@�-@�{@���@��@�hs@�X@��@���@��D@�9X@�1@��
@���@�t�@�C�@�"�@��!@�n�@��@��h@�`B@�?}@�V@�Q�@��
@��@�C�@��y@���@�~�@�V@�=q@�$�@��@��T@��^@�G�@��@��/@��@�1'@�1@��@���@��P@�S�@�@��R@�ff@�5?@���@�x�@�x�@�O�@��j@�r�@�(�@���@�dZ@��H@��\@�-@��@�{@���@�hs@��@���@�z�@� �@���@�C�@�
=@��@�@�O�@��@��/@���@��9@�j@�(�@� �@��@��
@��F@���@���@��@�dZ@�;d@�ȴ@y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A܏\A܍PAܙ�Aܙ�Aܛ�Aܛ�Aܝ�Aܝ�Aܟ�Aܡ�Aܥ�Aܥ�A�$�Aۗ�A�VA�$�A�A�ĜAڗ�A�{A�M�A��A�/A�O�AёhAЩ�AσA�S�A�bNA��A�XAť�A�JAÑhA�I�A���A��9A�JA�"�A� �A�VA�M�A��PA��FA�"�A�A��A���A�ĜA��A��TA�$�A��wA�C�A�
=A�A�{A��A��/A�%A�n�A��/A�n�A�%A��A�|�A��TA�/A�~�A�I�A���A���A�hsA�9XA�?}A�33A�C�A��`A���A�%A���A�t�A��wA�G�A���A�t�A��uA���A���A�t�A�z�A���A�K�A��-A�$�A�ȴA��mA��/A���A��A��uA�l�A��HA��PA�A�A���A��#A��9A�E�A���Azz�Av��Au��As�FAp�jAm/Af��Ad��Aa��AZĜAUx�AS
=AP^5AMƨAJA�AGt�AFQ�AC�^AB��A@��A>��A=��A;��A9��A6�HA5A4��A3��A2��A0�yA.��A.�A-�
A,�9A*ĜA)O�A(v�A'l�A&�HA&��A$�/A!�hA!33A ��A�A   A��AȴA�+A{A�hA�mA�`A��A��A�!A��AXA�An�A�#A�wA�-Av�AXA?}A��A5?A�TAl�A?}A"�A�A�A
��A�9A`BA��A�DA=qA%A��A`BAr�AM�At�A�AVA�+A�HAJAO�A�jAoA�uA;d@�l�@��9@��F@���@��`@�+@�X@�ȴ@��T@���@�"�@�P@���@���@�l�@���@�33@�S�@�F@�b@�j@�bN@�ȴ@�@�X@��@ܴ9@ۮ@�"�@ڸR@�M�@��@���@֗�@��@�A�@�^5@�$�@ѡ�@��@�X@�ȴ@�v�@��m@�K�@�hs@ȃ@�r�@�Z@��;@�
=@�b@�$�@��@��@�1@�1@���@���@�K�@�~�@�`B@���@��D@��F@�@��y@���@��@��-@�K�@��@�@��h@�/@�V@�bN@��@��w@��R@�=q@���@���@��w@��y@��!@�v�@���@��@�%@�b@�Q�@��;@��u@���@�O�@��#@�v�@���@���@��@�7L@� �@�l�@�^5@���@�M�@�l�@�@���@���@�
=@�S�@�dZ@�dZ@�
=@���@��@�C�@���@��F@��
@��@��@�;d@��R@��@��#@��@��7@�p�@���@��;@���@�C�@���@�~�@�n�@�~�@�V@�$�@�@�V@�Q�@�ƨ@�I�@�j@���@��j@���@�b@�\)@��w@�l�@�-@�@���@�`B@�x�@���@��@�J@��@��h@��@�1'@�@��y@��y@���@�o@���@�n�@��\@��+@�M�@�-@�{@���@��@�hs@�X@��@���@��D@�9X@�1@��
@���@�t�@�C�@�"�@��!@�n�@��@��h@�`B@�?}@�V@�Q�@��
@��@�C�@��y@���@�~�@�V@�=q@�$�@��@��T@��^@�G�@��@��/@��@�1'@�1@��@���@��P@�S�@�@��R@�ff@�5?@���@�x�@�x�@�O�@��j@�r�@�(�@���@�dZ@��H@��\@�-@��@�{@���@�hs@��@���@�z�@� �@���@�C�@�
=@��@�@�O�@��@��/@���@��9@�j@�(�@� �@��@��
@��F@���@���@��@�dZ@�;d@�ȴ@y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�/B��BȴBƨBŢBÖBB�}B�dB�FB�?B�?B�qB�}BB��BB{B,B49B;dBL�B[#B`BBhsBw�B}�B�B�7B�=B�=B�JB�{B�{B�hB�oB�uB�oB�bB�JB�7B�B� B}�Bw�Bw�Bw�Bv�Bu�Bq�BgmBcTB^5BS�BL�B=qB33B$�BhB+BB��B�B�HB��B�}B�B�\B~�Bx�Bp�BhsBW
BE�B;dB0!B#�B�BVB%B
��B
�B
�B
�ZB
�B
��B
��B
�'B
��B
��B
�VB
�7B
�B
~�B
{�B
x�B
n�B
bNB
5?B
�B
hB	��B	�ZB	��B	��B	�DB	v�B	M�B	.B	#�B	�B	B��B�B�B�HB�)B�
B��B��BȴBB�jB�XB�LB�?B�'B�B�B�B�B�B��B��B��B��B��B��B�hB�VB�PB�PB�1B�=B�7B�+B�1B�1B�PB�DB�%B�1B��B��B��B��B�hB��B��B�{B�oB�%Bq�BgmBe`Be`BdZBcTBbNBbNBcTBbNB`BB]/B\)BYBXBVBYBbNBl�B~�B��B��B�7B�B�=B�{B�bB�!B�B��B��B�DB}�Bq�Bs�Be`B`BB[#BR�BM�BM�BO�BR�BZB^5BZBM�BF�BH�BK�BN�BS�BffBk�Bm�Bk�BiyBdZBZBVBVBXBXBYB\)BXBVBXB]/B`BB`BBaHBgmBs�Bu�Bq�Be`BbNBbNBbNBcTBdZBcTBhsBk�Bl�Bl�Bp�Br�Bs�Br�Bq�Bu�Bz�B{�B|�B~�B� B�B�7B�7B�=B�{B�\B�1B�PB�\B�\B�bB�oB�uB��B��B��B��B��B��B��B��B��B�B�B�'B�?B��B��B��B��B�BɺB	+B	,B	/B	,B	(�B	'�B	%�B	&�B	-B	7LB	;dB	=qB	A�B	I�B	M�B	R�B	S�B	T�B	VB	]/B	ffB	m�B	o�B	s�B	z�B	�B	�B	� B	� B	�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�7B	�=B	�DB	�DB	�DB	�=B	�7B	�DB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�^B	�qB	�wB	�wB	�wB	�}B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�;B	�BB	�BB	�HB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
{B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�B�B�B�B�B�B�B�B�B�B�/B��BȴBƨBŢBÖBB�}B�dB�FB�?B�?B�qB�}BB��BB{B,B49B;dBL�B[#B`BBhsBw�B}�B�B�7B�=B�=B�JB�{B�{B�hB�oB�uB�oB�bB�JB�7B�B� B}�Bw�Bw�Bw�Bv�Bu�Bq�BgmBcTB^5BS�BL�B=qB33B$�BhB+BB��B�B�HB��B�}B�B�\B~�Bx�Bp�BhsBW
BE�B;dB0!B#�B�BVB%B
��B
�B
�B
�ZB
�B
��B
��B
�'B
��B
��B
�VB
�7B
�B
~�B
{�B
x�B
n�B
bNB
5?B
�B
hB	��B	�ZB	��B	��B	�DB	v�B	M�B	.B	#�B	�B	B��B�B�B�HB�)B�
B��B��BȴBB�jB�XB�LB�?B�'B�B�B�B�B�B��B��B��B��B��B��B�hB�VB�PB�PB�1B�=B�7B�+B�1B�1B�PB�DB�%B�1B��B��B��B��B�hB��B��B�{B�oB�%Bq�BgmBe`Be`BdZBcTBbNBbNBcTBbNB`BB]/B\)BYBXBVBYBbNBl�B~�B��B��B�7B�B�=B�{B�bB�!B�B��B��B�DB}�Bq�Bs�Be`B`BB[#BR�BM�BM�BO�BR�BZB^5BZBM�BF�BH�BK�BN�BS�BffBk�Bm�Bk�BiyBdZBZBVBVBXBXBYB\)BXBVBXB]/B`BB`BBaHBgmBs�Bu�Bq�Be`BbNBbNBbNBcTBdZBcTBhsBk�Bl�Bl�Bp�Br�Bs�Br�Bq�Bu�Bz�B{�B|�B~�B� B�B�7B�7B�=B�{B�\B�1B�PB�\B�\B�bB�oB�uB��B��B��B��B��B��B��B��B��B�B�B�'B�?B��B��B��B��B�BɺB	+B	,B	/B	,B	(�B	'�B	%�B	&�B	-B	7LB	;dB	=qB	A�B	I�B	M�B	R�B	S�B	T�B	VB	]/B	ffB	m�B	o�B	s�B	z�B	�B	�B	� B	� B	�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�7B	�=B	�DB	�DB	�DB	�=B	�7B	�DB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�^B	�qB	�wB	�wB	�wB	�}B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�;B	�BB	�BB	�HB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
{B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191706                              AO  ARCAADJP                                                                    20181005191706    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191706  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191706  QCF$                G�O�G�O�G�O�8000            