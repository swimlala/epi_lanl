CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-19T18:46:18Z creation;2023-07-19T18:46:25Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230719184618  20230719185844  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�;��2�1   @�;�d��@4���vȴ�c�(�\1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW�fDXfDX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�D`��Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dly�Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�C3D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр DѼ�D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D��3D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�3D��3D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D���D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�A�HA&�HAEG�Af�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB
�B�RB�RB!�RB)�RB1�RB9�RBB�BI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B�\B�\B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"TzC$TzC&TzC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCH��CJnCLnCNnCPnCRnCTTzCVnCXnCZnC\nC^nC`nCbnCdnCfnChTzCjnClnCnnCpnCrTzCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>!�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD!�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW!�DW��DX!�DX��DY!�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`�DaDa��Db�Db��Dc�Dc��Dd�Dd��De!�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�Dl�Dl�Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�M�D���D���D��D�P�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D�ʐD��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D�
�D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D�
�D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D�
�D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D�ʐD��D�P�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D�D���D��D�M�DÍ�D���D��D�M�Dč�D���D��D�M�Dō�D���D��D�M�Dƍ�D���D��D�M�DǍ�D�ʐD��D�M�Dȍ�D���D��D�M�Dɍ�D���D��D�M�Dʍ�D���D��D�M�Dˍ�D���D��D�P�D̐�D���D��D�M�D͍�D���D��D�M�D΍�D���D��D�M�Dύ�D���D��D�M�DЍ�D���D��D�M�Dэ�D�ʐD��D�M�Dҍ�D���D��D�M�DӍ�D���D��D�M�Dԍ�D���D��D�M�DՍ�D���D��D�M�D֍�D���D��D�M�D׍�D���D��D�M�D؍�D���D��D�M�Dٍ�D���D��D�M�Dڍ�D���D��D�M�Dۍ�D���D��D�M�D܍�D���D��D�M�Dݍ�D���D��D�M�Dލ�D���D��D�M�Dߍ�D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D�ʐD��D�M�D��D���D��D�M�D��D���D�
�D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�P�D��D�ʐD��D�M�D��D���D��D�P�D��D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�J�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�P�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A�"�A�&�A�+A�(�A�&�A�+A�+A�+A�-A��AӰ!AӶFA�$�A�A��yA��Aȡ�A�dZAƅAş�AļjA�S�Aº^A�  A��A�9XA�
=A��9A�=qA���A�l�A�1'A��+A�Q�A��!A��hA��A���A��
A�ffA�z�A�`BA��hA��/A��wA�7LA�Q�A�x�A���A�ZA�VA�hsA�Q�A��+A�ZA�5?A�~�A���A�ƨA�XA���A�r�A�ĜA�-A���A���A���A�XA���A�5?A�9XA��wA��A�G�A��A��hA�1A��A�ĜA���A��A�ȴA�A�A��A���A�1A��A�7LA���A��yA��Ap�A{��Ay"�AwS�AsO�Apn�Ak�AhjAfjAe��AeS�Ac�-A_l�A\ĜAY��AX��AXjAVbNATM�AQG�APbNAO�AN^5AK�AJr�AI��AH(�AF^5AE+AD1'ACdZAC&�AB�uAA/A=t�A:ffA8z�A7�^A5`BA3;dA.��A,(�A+x�A)��A'��A'�A&$�A%hsA#�A �9A�uA��A��A�-AK�Az�A�A%A(�A&�A�A��A+A�yA��AffAC�A��Az�A�FA�!A�7A�A�/AĜA�A��A5?AhsA�#A{Al�A
I�A	��A	"�AffA�Az�A�;A��A��A�A+A�uA��A�+A`BA ȴ@���@��!@�X@���@�j@�33@�ȴ@��@��9@�ȴ@���@��@�+@��#@�A�@���@�+@���@���@�Ĝ@�t�@��H@ꟾ@�%@睲@�^5@�@��`@�A�@���@���@��@㕁@�@�o@�9X@ݡ�@�`B@�/@���@�bN@�ȴ@�hs@�Z@�+@�{@��#@�?}@�5?@��@д9@�1'@�b@�1@���@�
=@��/@�Q�@�
=@�;d@�@�{@�7L@ȣ�@�A�@��@ǥ�@�dZ@�+@��@ư!@�^5@��@�hs@��@���@�Ĝ@�z�@�A�@��;@î@�;d@���@�@\@�^5@��@�O�@�r�@���@�;d@�ȴ@��\@�M�@�@���@��-@�&�@���@��`@��@��D@�j@�Z@�A�@�(�@�1@�ƨ@�|�@�K�@�
=@��@��!@�=q@���@�%@� �@���@�l�@�C�@�@��!@�v�@�=q@���@�@�hs@�/@��@��@�t�@��H@��!@�v�@�=q@��@�hs@��@���@�(�@��@�S�@�;d@��@���@�-@�V@�Q�@�1@�|�@�@��@���@�^5@��#@��h@�X@��@��@��@�Q�@�1'@��w@�@��\@�5?@��@��^@�p�@��`@�z�@�1@���@�C�@�"�@�
=@�ȴ@�^5@�5?@���@��-@�p�@�?}@��u@�9X@��
@��@�\)@�"�@��\@�n�@�=q@�$�@�{@�@��@��T@���@��@��@�Z@� �@��m@��F@��@�dZ@�C�@�"�@���@��H@�v�@��@�J@��T@��7@�?}@��@���@��j@��@�r�@�I�@� �@�b@�ƨ@���@�l�@�;d@��H@�~�@�n�@�=q@�@���@�&�@�j@�I�@�(�@��;@�ƨ@�ƨ@�ƨ@��@��P@�t�@�S�@��@���@��\@�^5@�V@�=q@�-@��#@�p�@��@��@��9@�Q�@��F@�\)@��y@���@�~�@�=q@���@��h@�p�@�`B@�O�@�G�@�%@�Ĝ@�Z@�1@��F@��@�\)@�+@��@���@���@�E�@��h@�&�@��@�Ĝ@��D@�1@��
@��F@��@��@��@�l�@�\)@�C�@�;d@�C�@�S�@�\)@�\)@�\)@�+@��H@��R@�~�@�5?@�@���@��@�G�@��@��j@��D@�Z@�9X@���@���@�|�@�33@�
=@���@��@���@��-@�O�@�/@�&�@�%@���@��@� �@�P@+@~��@~��@~�y@~�+@~{@}��@}�-@}�@}O�@|��@|�D@|z�@|1@{t�@z��@y��@y��@yX@y7L@y�@y%@x�9@xb@w�P@v�y@v�y@v�y@v�+@u�@up�@u�@t�@t�@t�@tj@tZ@t�@t1@s��@s�m@s�F@so@r�\@r-@q�^@q��@qx�@qhs@q7L@q�@p��@pr�@o�w@o|�@o;d@o�@o
=@o
=@n�y@n�y@nȴ@nff@m@mp�@m`B@mO�@m�@l9X@l1@k�
@kdZ@j�@jJ@i�7@iX@i�@h��@h��@hr�@h1'@h  @g�P@g�@f��@fff@f$�@e�T@e@e�@eV@d�@c��@cƨ@c@b��@bM�@ax�@`��@`�@`1'@_�P@^@\z�@\�@[��@Z~�@Z=q@Y��@Y�#@Y�@X�u@X1'@W�w@V�@V5?@U�@U@U�@Up�@UO�@UV@T�j@T�D@T1@S�@SC�@So@So@S@R��@R�\@RM�@Q��@Q7L@P�9@O�@O|�@O+@O�@N��@N�y@Nȴ@N��@Nff@N@M/@L��@L�j@LZ@L1@K�@KS�@K33@K"�@K"�@K@J^5@JJ@I��@IG�@I�@H��@H1'@Hb@H  @G�@G;d@G
=@Fȴ@FV@F{@E�@E��@E�@D��@Dj@Dj@Dj@D9X@Co@B=q@A�#@A��@AG�@A7L@A�@@��@@�@@b@?�;@?��@?�P@?�P@?�P@?|�@?l�@?\)@?;d@>�y@>ff@=�T@=/@<�D@<(�@<1@;"�@;@:�@:��@:�!@:��@:~�@:�@9��@9x�@9G�@9�@8�9@8Q�@7�;@7�;@7�@7+@7�@6��@6V@65?@6{@5�@5@5O�@4�/@4�D@4j@3��@3��@3��@3��@3t�@3C�@2�H@2��@2��@2�!@2��@2^5@2-@2�@1��@1�#@1��@1�^@1��@1x�@1X@1&�@0�u@0b@0  @/�@/�@/�@/��@/|�@/l�@/|�@/l�@/K�@/�@.�y@.�R@.��@.v�@.V@.E�@.$�@-�@-�h@-p�@-O�@-?}@-?}@-?}@-?}@-/@,�j@,z�@,�@+�F@+�@+dZ@+C�@*��@*�\@*-@*�@)�^@(��@(bN@(A�@(b@'��@'�@'l�@&�y@&�+@&$�@%�@%@%�-@%��@%`B@%/@$��@$�@$�@$�@$�/@$��@$�j@$�@$z�@$Z@#�m@#��@#t�@#dZ@#dZ@#S�@#C�@#"�@#o@"��@"�!@"�!@"�\@"n�@"=q@!��@!��@!�7@!X@ ��@ �9@ �u@ �u@ �u@ ��@ ��@ r�@ Q�@ Q�@�w@�w@�@��@��@\)@+@
=@�R@v�@$�@�-@p�@/@V@��@��@�@�j@�D@Z@(�@�m@�F@��@t�@��@��@�#@�#@�#@��@�^@��@X@%@��@��@bN@�@\)@ȴ@ff@{@�T@�-@?}@�@��@��@Z@�@�
@�F@@��@�\@=q@�@�@�7@7L@&�@%@��@�u@�@�@1'@  @�;@�@�P@l�@K�@;d@+@+@�@
=@��@�y@�+@@��@p�@`B@?}@�@�@�j@�j@�@��@��@j@(�@�@�@�m@�F@��@
�@
~�@
=q@
J@	��@	��@	��@	��@	�@	�@	��@	�@��@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A�"�A�&�A�+A�(�A�&�A�+A�+A�+A�-A��AӰ!AӶFA�$�A�A��yA��Aȡ�A�dZAƅAş�AļjA�S�Aº^A�  A��A�9XA�
=A��9A�=qA���A�l�A�1'A��+A�Q�A��!A��hA��A���A��
A�ffA�z�A�`BA��hA��/A��wA�7LA�Q�A�x�A���A�ZA�VA�hsA�Q�A��+A�ZA�5?A�~�A���A�ƨA�XA���A�r�A�ĜA�-A���A���A���A�XA���A�5?A�9XA��wA��A�G�A��A��hA�1A��A�ĜA���A��A�ȴA�A�A��A���A�1A��A�7LA���A��yA��Ap�A{��Ay"�AwS�AsO�Apn�Ak�AhjAfjAe��AeS�Ac�-A_l�A\ĜAY��AX��AXjAVbNATM�AQG�APbNAO�AN^5AK�AJr�AI��AH(�AF^5AE+AD1'ACdZAC&�AB�uAA/A=t�A:ffA8z�A7�^A5`BA3;dA.��A,(�A+x�A)��A'��A'�A&$�A%hsA#�A �9A�uA��A��A�-AK�Az�A�A%A(�A&�A�A��A+A�yA��AffAC�A��Az�A�FA�!A�7A�A�/AĜA�A��A5?AhsA�#A{Al�A
I�A	��A	"�AffA�Az�A�;A��A��A�A+A�uA��A�+A`BA ȴ@���@��!@�X@���@�j@�33@�ȴ@��@��9@�ȴ@���@��@�+@��#@�A�@���@�+@���@���@�Ĝ@�t�@��H@ꟾ@�%@睲@�^5@�@��`@�A�@���@���@��@㕁@�@�o@�9X@ݡ�@�`B@�/@���@�bN@�ȴ@�hs@�Z@�+@�{@��#@�?}@�5?@��@д9@�1'@�b@�1@���@�
=@��/@�Q�@�
=@�;d@�@�{@�7L@ȣ�@�A�@��@ǥ�@�dZ@�+@��@ư!@�^5@��@�hs@��@���@�Ĝ@�z�@�A�@��;@î@�;d@���@�@\@�^5@��@�O�@�r�@���@�;d@�ȴ@��\@�M�@�@���@��-@�&�@���@��`@��@��D@�j@�Z@�A�@�(�@�1@�ƨ@�|�@�K�@�
=@��@��!@�=q@���@�%@� �@���@�l�@�C�@�@��!@�v�@�=q@���@�@�hs@�/@��@��@�t�@��H@��!@�v�@�=q@��@�hs@��@���@�(�@��@�S�@�;d@��@���@�-@�V@�Q�@�1@�|�@�@��@���@�^5@��#@��h@�X@��@��@��@�Q�@�1'@��w@�@��\@�5?@��@��^@�p�@��`@�z�@�1@���@�C�@�"�@�
=@�ȴ@�^5@�5?@���@��-@�p�@�?}@��u@�9X@��
@��@�\)@�"�@��\@�n�@�=q@�$�@�{@�@��@��T@���@��@��@�Z@� �@��m@��F@��@�dZ@�C�@�"�@���@��H@�v�@��@�J@��T@��7@�?}@��@���@��j@��@�r�@�I�@� �@�b@�ƨ@���@�l�@�;d@��H@�~�@�n�@�=q@�@���@�&�@�j@�I�@�(�@��;@�ƨ@�ƨ@�ƨ@��@��P@�t�@�S�@��@���@��\@�^5@�V@�=q@�-@��#@�p�@��@��@��9@�Q�@��F@�\)@��y@���@�~�@�=q@���@��h@�p�@�`B@�O�@�G�@�%@�Ĝ@�Z@�1@��F@��@�\)@�+@��@���@���@�E�@��h@�&�@��@�Ĝ@��D@�1@��
@��F@��@��@��@�l�@�\)@�C�@�;d@�C�@�S�@�\)@�\)@�\)@�+@��H@��R@�~�@�5?@�@���@��@�G�@��@��j@��D@�Z@�9X@���@���@�|�@�33@�
=@���@��@���@��-@�O�@�/@�&�@�%@���@��@� �@�P@+@~��@~��@~�y@~�+@~{@}��@}�-@}�@}O�@|��@|�D@|z�@|1@{t�@z��@y��@y��@yX@y7L@y�@y%@x�9@xb@w�P@v�y@v�y@v�y@v�+@u�@up�@u�@t�@t�@t�@tj@tZ@t�@t1@s��@s�m@s�F@so@r�\@r-@q�^@q��@qx�@qhs@q7L@q�@p��@pr�@o�w@o|�@o;d@o�@o
=@o
=@n�y@n�y@nȴ@nff@m@mp�@m`B@mO�@m�@l9X@l1@k�
@kdZ@j�@jJ@i�7@iX@i�@h��@h��@hr�@h1'@h  @g�P@g�@f��@fff@f$�@e�T@e@e�@eV@d�@c��@cƨ@c@b��@bM�@ax�@`��@`�@`1'@_�P@^@\z�@\�@[��@Z~�@Z=q@Y��@Y�#@Y�@X�u@X1'@W�w@V�@V5?@U�@U@U�@Up�@UO�@UV@T�j@T�D@T1@S�@SC�@So@So@S@R��@R�\@RM�@Q��@Q7L@P�9@O�@O|�@O+@O�@N��@N�y@Nȴ@N��@Nff@N@M/@L��@L�j@LZ@L1@K�@KS�@K33@K"�@K"�@K@J^5@JJ@I��@IG�@I�@H��@H1'@Hb@H  @G�@G;d@G
=@Fȴ@FV@F{@E�@E��@E�@D��@Dj@Dj@Dj@D9X@Co@B=q@A�#@A��@AG�@A7L@A�@@��@@�@@b@?�;@?��@?�P@?�P@?�P@?|�@?l�@?\)@?;d@>�y@>ff@=�T@=/@<�D@<(�@<1@;"�@;@:�@:��@:�!@:��@:~�@:�@9��@9x�@9G�@9�@8�9@8Q�@7�;@7�;@7�@7+@7�@6��@6V@65?@6{@5�@5@5O�@4�/@4�D@4j@3��@3��@3��@3��@3t�@3C�@2�H@2��@2��@2�!@2��@2^5@2-@2�@1��@1�#@1��@1�^@1��@1x�@1X@1&�@0�u@0b@0  @/�@/�@/�@/��@/|�@/l�@/|�@/l�@/K�@/�@.�y@.�R@.��@.v�@.V@.E�@.$�@-�@-�h@-p�@-O�@-?}@-?}@-?}@-?}@-/@,�j@,z�@,�@+�F@+�@+dZ@+C�@*��@*�\@*-@*�@)�^@(��@(bN@(A�@(b@'��@'�@'l�@&�y@&�+@&$�@%�@%@%�-@%��@%`B@%/@$��@$�@$�@$�@$�/@$��@$�j@$�@$z�@$Z@#�m@#��@#t�@#dZ@#dZ@#S�@#C�@#"�@#o@"��@"�!@"�!@"�\@"n�@"=q@!��@!��@!�7@!X@ ��@ �9@ �u@ �u@ �u@ ��@ ��@ r�@ Q�@ Q�@�w@�w@�@��@��@\)@+@
=@�R@v�@$�@�-@p�@/@V@��@��@�@�j@�D@Z@(�@�m@�F@��@t�@��@��@�#@�#@�#@��@�^@��@X@%@��@��@bN@�@\)@ȴ@ff@{@�T@�-@?}@�@��@��@Z@�@�
@�F@@��@�\@=q@�@�@�7@7L@&�@%@��@�u@�@�@1'@  @�;@�@�P@l�@K�@;d@+@+@�@
=@��@�y@�+@@��@p�@`B@?}@�@�@�j@�j@�@��@��@j@(�@�@�@�m@�F@��@
�@
~�@
=q@
J@	��@	��@	��@	��@	�@	�@	��@	�@��@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�mB�sB�sB�fB�fB�B	7B"�B�B�B�BS�B}�B�%B�PB�=B�uB��B��B��B��B��B��B�Bx�B|�Br�Bm�BbNBO�B8RB;dBk�BYB/B,B(�B�B\BB��B��B�HB��BB��B��B��B�)B�)B�#B�B��B��B�?B�B�B��B�VB�Bq�BaHBM�BA�B7LB&�B\B
�B
�B
�yB
�sB
�TB
��B
�qB
��B
|�B
jB
]/B
R�B
N�B
-B
)�B
)�B
�B
%B	��B	�`B	��B	�FB	��B	��B	�uB	�\B	�B	p�B	aHB	Q�B	F�B	D�B	=qB	33B	(�B	"�B	�B	�B	hB		7B	B	B��B��B�B�B�B�B�fB�)B��BǮBĜB��B�qB�dB�-B�!B�-B�FB�3B�'B�B��B�uB�PB�+B�1B�1B�7B�7B�PB�VB�VB�PB�JB�=B�=B�7B�1B�+B�7B�+B�%B�%B�B�=B�DB�DB�PB�\B�oB��B��B�'B�qB�^B�!B�B�B�B�B�!B�B�?B�LB�LB�LB�?B�3B�!B�B�'B�B�9B�FB�dB�}BȴB��B��B��B�B�
B�B�5B�BB�NB�NB�ZB�fB�mB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	+B	1B	1B	1B	1B	+B	
=B	
=B	DB	JB	
=B		7B	JB	\B	\B	bB	hB	bB	bB	bB	uB	�B	�B	'�B	49B	:^B	=qB	>wB	A�B	G�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	P�B	S�B	T�B	T�B	VB	W
B	XB	ZB	[#B	]/B	_;B	_;B	_;B	_;B	aHB	dZB	ffB	hsB	jB	k�B	l�B	m�B	n�B	o�B	p�B	s�B	t�B	u�B	w�B	x�B	y�B	y�B	z�B	{�B	{�B	}�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�dB	�qB	�wB	��B	��B	B	B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�)B	�/B	�;B	�BB	�HB	�HB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
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
	7B
DB
VB
VB
\B
bB
bB
bB
bB
hB
hB
hB
hB
uB
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
+B
+B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
7LB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�mB�sB�sB�fB�fB�B	7B"�B�B�B�BS�B}�B�%B�PB�=B�uB��B��B��B��B��B��B�Bx�B|�Br�Bm�BbNBO�B8RB;dBk�BYB/B,B(�B�B\BB��B��B�HB��BB��B��B��B�)B�)B�#B�B��B��B�?B�B�B��B�VB�Bq�BaHBM�BA�B7LB&�B\B
�B
�B
�yB
�sB
�TB
��B
�qB
��B
|�B
jB
]/B
R�B
N�B
-B
)�B
)�B
�B
%B	��B	�`B	��B	�FB	��B	��B	�uB	�\B	�B	p�B	aHB	Q�B	F�B	D�B	=qB	33B	(�B	"�B	�B	�B	hB		7B	B	B��B��B�B�B�B�B�fB�)B��BǮBĜB��B�qB�dB�-B�!B�-B�FB�3B�'B�B��B�uB�PB�+B�1B�1B�7B�7B�PB�VB�VB�PB�JB�=B�=B�7B�1B�+B�7B�+B�%B�%B�B�=B�DB�DB�PB�\B�oB��B��B�'B�qB�^B�!B�B�B�B�B�!B�B�?B�LB�LB�LB�?B�3B�!B�B�'B�B�9B�FB�dB�}BȴB��B��B��B�B�
B�B�5B�BB�NB�NB�ZB�fB�mB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	+B	1B	1B	1B	1B	+B	
=B	
=B	DB	JB	
=B		7B	JB	\B	\B	bB	hB	bB	bB	bB	uB	�B	�B	'�B	49B	:^B	=qB	>wB	A�B	G�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	P�B	S�B	T�B	T�B	VB	W
B	XB	ZB	[#B	]/B	_;B	_;B	_;B	_;B	aHB	dZB	ffB	hsB	jB	k�B	l�B	m�B	n�B	o�B	p�B	s�B	t�B	u�B	w�B	x�B	y�B	y�B	z�B	{�B	{�B	}�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�dB	�qB	�wB	��B	��B	B	B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�)B	�/B	�;B	�BB	�HB	�HB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
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
	7B
DB
VB
VB
\B
bB
bB
bB
bB
hB
hB
hB
hB
uB
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
+B
+B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
7LB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230720034609  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230719184618  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230719184624  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230719184625                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230719184627  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230719184627  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230719185844                      G�O�G�O�G�O�                