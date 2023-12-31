CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-03T00:50:38Z creation;2023-02-03T00:50:39Z conversion to V3.1      
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  s(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ː   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230203005038  20230203010716  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @��ߒ��1   @����-�@5,������c�M���1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D�fDfD�fD  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D��3D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D��3D�  D�@ D̀ D�� D�3D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�<�Dр D�� D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ D�|�D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D���D�@ D�3D��3D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B�\B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCTzCTzCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`TzCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|��C~��C�7
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
C�C�C�C�C�7
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D!�D��D�D��D�D��D!�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.�D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4�D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DNDN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db!�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��DhDh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��DtDt��Du�Du��Dv�Dv��Dw!�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�M�D���D�ʐD��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D�
�D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D�
�D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�J�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D�ʐD��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D�D���D��D�M�DÍ�D���D��D�M�Dč�D���D��D�M�Dō�D���D��D�M�Dƍ�D���D�
�D�M�DǍ�D���D��D�M�Dȍ�D���D��D�M�Dɍ�D���D��D�M�Dʍ�D���D��D�M�Dˍ�D���D��D�M�D̍�D���D��D�M�D͍�D���D��D�M�D΍�D���D��D�M�Dύ�D���D��D�M�DЍ�D���D��D�J�Dэ�D���D��D�M�Dҍ�D���D��D�M�DӍ�D���D��D�M�Dԍ�D���D��D�M�DՍ�D���D��D�M�D֍�D���D��D�M�D׍�D���D��D�M�D؍�D���D��D�M�Dٍ�D���D��D�M�Dڊ�D���D��D�M�Dۍ�D���D��D�M�D܍�D���D��D�M�Dݍ�D���D��D�M�Dލ�D���D��D�M�Dߍ�D���D��D�M�D���D���D��D�M�D��D���D��D�M�D⊐D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�DꊐD���D��D�M�D��D���D�
�D�M�D��D���D��D�P�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�P�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��`A��/A��
A���Aİ!AđhAđhAčPAăA�|�A�v�A�v�A�n�A�z�A�t�Aď\Aę�Aĩ�A���A��
A��#A��;A��A���A�1A�+A�=qA�S�A�^5A�9XA��/A�bNA�"�Aº^A�A��
A�ffA��RA���A��A���A���A��A�dZA��A��+A�1A���A��jA�I�A���A���A��+A�?}A�t�A���A���A�S�A��\A��A�"�A���A��A���A�E�A��!A�C�A��!A�E�A���A�O�A�A��PA���A��+A���A��A�;dA�v�A��
A��7A���A�%A���A�JA���A�v�A�ȴA�I�A�JA��A��A�$�At�A{�-A{
=AzM�Ay�;Ay%AxbNAvVAu%At��At�!As"�Ao&�Aix�Ag��Af��Ad-Ab�HA`��AZz�AQXAP9XAN��AM;dAI`BAG��AG;dAE+AC33AB��AB�+AA��A?��A=�A<E�A:�RA9��A6�9A57LA4E�A3+A1&�A/A-33A+;dA)�PA(��A&1'A%O�A$�A$��A$�DA$ffA#&�A!+A �9A�A�A�\AbA�PA��A1A��A�hAp�A�A7LAXA��A��A�HA��A=qA�wA�FA&�AJA�7A^5A�AK�A��A5?AƨAp�A
�9A	�A	S�A��A��AS�A�
A�7A;dA�AG�A/A �@�ȴ@�`B@���@��u@�ƨ@�z�@��#@��@���@���@�7@���@�w@�5?@�j@�D@�(�@�+@�h@���@��@���@���@�X@�%@���@�z�@�  @�K�@�@���@ާ�@�M�@�J@ݩ�@�x�@�O�@���@���@��y@�E�@��#@١�@�p�@؃@ָR@�@ՙ�@�?}@Լj@�S�@҇+@�M�@�M�@��#@���@� �@��@�@�x�@�G�@�Z@�dZ@ʏ\@��T@�hs@�X@�/@���@ȼj@ȋD@�Z@�(�@�b@Ǿw@�33@��@���@�Q�@�(�@���@þw@�33@���@�p�@�?}@���@���@���@�1'@��@���@�S�@�
=@��@��y@��!@�$�@�O�@��@�9X@��@��@��w@��P@�dZ@�;d@���@��@��7@�V@��@�9X@�t�@�"�@��y@��!@�ff@��^@��h@��@�X@���@�1@���@��P@�+@��H@���@�@��7@�p�@�?}@��u@�A�@���@���@�dZ@�33@��@���@��@�?}@��@���@��/@��@�j@�(�@�  @��w@�dZ@�"�@�@���@�$�@���@���@��7@�`B@�X@�%@�Z@�A�@�b@���@�"�@���@��`@�A�@��@�C�@���@�ȴ@��\@���@���@���@���@�r�@�I�@�(�@�ƨ@���@���@�~�@�{@��@��T@��T@��T@�@�O�@���@��9@���@�z�@�A�@��
@�|�@�o@���@�v�@�$�@�{@�O�@�Ĝ@��D@� �@��m@��@�|�@�\)@�33@�o@�@��@��!@�ff@�5?@��^@�7L@���@�Ĝ@��@�(�@�b@��;@��P@�C�@���@�n�@�ff@�^5@��#@��h@�`B@�?}@�7L@�/@�&�@�&�@�&�@���@��@��`@���@��9@��u@�A�@��@��F@�dZ@��@���@��!@�v�@�{@���@��h@���@���@�r�@�Z@�(�@��@��@��m@��;@��;@���@�ƨ@��@��@�l�@�S�@�C�@�33@�+@�o@�
=@�ȴ@���@�ff@�=q@�{@�@�@�7L@���@��j@��@�j@�Z@�I�@�9X@�1'@� �@�b@���@��
@��F@���@��@�dZ@�;d@��@��H@�{@���@�hs@�`B@�G�@�7L@�/@�/@�/@�&�@�&�@��@��@��@���@�Ĝ@�z�@��;@��P@�\)@�o@��@�^5@�=q@���@�@�hs@���@���@���@��@�1@\)@~ȴ@~@}�@}V@|�@{��@{33@z��@y��@x��@xA�@wl�@w
=@v�@v@u��@t�/@t(�@so@r�@qhs@p��@p�u@pr�@pQ�@p �@p  @o�P@nff@m��@m�@m�@m�@mV@l��@l�D@l�D@k�
@j��@ix�@i%@hĜ@h��@h1'@g�@g�P@f�y@e�@e`B@d��@d�j@d�D@dj@dI�@d9X@dI�@d9X@d9X@c��@c��@ct�@c"�@b�!@b�!@b^5@bM�@a��@a�@a�@a�^@a7L@`1'@_��@_��@_�@^��@^E�@^$�@]�@]�-@]�h@]`B@]`B@]?}@]?}@]�@\��@\1@[S�@Z��@Z�\@Zn�@Z^5@Z-@Y��@Y��@Yx�@YX@X��@X�u@XbN@W�;@W�@V�@V��@Vv�@VE�@V$�@V$�@V@V@V@U��@U�-@Up�@U�@UV@T�j@T�@Tz�@S��@Sƨ@S��@S��@S�@SdZ@SC�@So@R��@RM�@Q�@Q��@Qx�@Q�@P�`@P�9@Pr�@PA�@O�w@O|�@OK�@O�@N�y@N�R@N�+@N5?@M�h@M/@L�/@Lj@K�F@K�@K�@Kt�@KS�@KC�@J~�@I��@Ihs@I%@H  @Gl�@G\)@GK�@F�@E�@EO�@Dz�@CS�@C33@C@B��@B�\@Bn�@B�@A�^@Ax�@@��@@bN@@b@?��@?;d@?+@?�@?
=@>��@>�y@>�@>��@>v�@>5?@=�T@=p�@=/@<�/@<��@<�D@<j@<9X@;��@;�
@;dZ@;o@:�H@:��@:~�@:�@9��@9��@9x�@9hs@97L@9%@8�`@8��@8Q�@8  @7�;@7�;@7�;@7��@7��@7��@7�w@7�@7��@7l�@7
=@6�R@65?@5�@5��@5�@5/@5V@4��@4�/@4��@4��@4�D@49X@3��@3o@2�!@1��@1hs@1&�@1%@0��@0��@0�`@0��@0�9@0�u@0�u@0r�@0A�@/�@/+@.�@.�R@.$�@-�-@-��@-`B@-/@-V@-V@,��@,��@,�D@,z�@,j@,�@+�m@+��@+S�@+@*�!@*^5@)��@)�^@)X@(�`@(Ĝ@(�u@(  @'�;@'��@'�@'K�@';d@';d@'
=@&�R@&v�@&V@&E�@&5?@&{@%�T@%�h@%?}@$��@$��@$Z@#��@#t�@#S�@#33@#o@"�@"��@"n�@"=q@"J@!�^@!&�@ Ĝ@ �u@ Q�@ A�@ 1'@ 1'@  �@ b@   @�;@�@\)@
=@��@ȴ@�R@��@{@��@�-@��@�@V@�/@�j@�@j@(�@��@t�@33@�H@�!@�\@�@�#@��@��@X@&�@%@��@�@�@r�@1'@ �@b@��@\)@��@��@ff@5?@{@@��@��@��@��@��@�-@/@�@�j@I�@1@ƨ@ƨ@S�@�@��@��@M�@-@J@�^@��@��@X@��@�9@�u@r�@1'@b@b@�;@|�@\)@+@��@�@��@E�@{@��@�-@�@?}@�@V@��@�@�/@��@�D@j@I�@1@�
@��@t�@t�@dZ@33@o@
��@
~�@
n�@
M�@
�@
J@
J@
J@	�@	��@	�^@	��@	��@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��`A��/A��
A���Aİ!AđhAđhAčPAăA�|�A�v�A�v�A�n�A�z�A�t�Aď\Aę�Aĩ�A���A��
A��#A��;A��A���A�1A�+A�=qA�S�A�^5A�9XA��/A�bNA�"�Aº^A�A��
A�ffA��RA���A��A���A���A��A�dZA��A��+A�1A���A��jA�I�A���A���A��+A�?}A�t�A���A���A�S�A��\A��A�"�A���A��A���A�E�A��!A�C�A��!A�E�A���A�O�A�A��PA���A��+A���A��A�;dA�v�A��
A��7A���A�%A���A�JA���A�v�A�ȴA�I�A�JA��A��A�$�At�A{�-A{
=AzM�Ay�;Ay%AxbNAvVAu%At��At�!As"�Ao&�Aix�Ag��Af��Ad-Ab�HA`��AZz�AQXAP9XAN��AM;dAI`BAG��AG;dAE+AC33AB��AB�+AA��A?��A=�A<E�A:�RA9��A6�9A57LA4E�A3+A1&�A/A-33A+;dA)�PA(��A&1'A%O�A$�A$��A$�DA$ffA#&�A!+A �9A�A�A�\AbA�PA��A1A��A�hAp�A�A7LAXA��A��A�HA��A=qA�wA�FA&�AJA�7A^5A�AK�A��A5?AƨAp�A
�9A	�A	S�A��A��AS�A�
A�7A;dA�AG�A/A �@�ȴ@�`B@���@��u@�ƨ@�z�@��#@��@���@���@�7@���@�w@�5?@�j@�D@�(�@�+@�h@���@��@���@���@�X@�%@���@�z�@�  @�K�@�@���@ާ�@�M�@�J@ݩ�@�x�@�O�@���@���@��y@�E�@��#@١�@�p�@؃@ָR@�@ՙ�@�?}@Լj@�S�@҇+@�M�@�M�@��#@���@� �@��@�@�x�@�G�@�Z@�dZ@ʏ\@��T@�hs@�X@�/@���@ȼj@ȋD@�Z@�(�@�b@Ǿw@�33@��@���@�Q�@�(�@���@þw@�33@���@�p�@�?}@���@���@���@�1'@��@���@�S�@�
=@��@��y@��!@�$�@�O�@��@�9X@��@��@��w@��P@�dZ@�;d@���@��@��7@�V@��@�9X@�t�@�"�@��y@��!@�ff@��^@��h@��@�X@���@�1@���@��P@�+@��H@���@�@��7@�p�@�?}@��u@�A�@���@���@�dZ@�33@��@���@��@�?}@��@���@��/@��@�j@�(�@�  @��w@�dZ@�"�@�@���@�$�@���@���@��7@�`B@�X@�%@�Z@�A�@�b@���@�"�@���@��`@�A�@��@�C�@���@�ȴ@��\@���@���@���@���@�r�@�I�@�(�@�ƨ@���@���@�~�@�{@��@��T@��T@��T@�@�O�@���@��9@���@�z�@�A�@��
@�|�@�o@���@�v�@�$�@�{@�O�@�Ĝ@��D@� �@��m@��@�|�@�\)@�33@�o@�@��@��!@�ff@�5?@��^@�7L@���@�Ĝ@��@�(�@�b@��;@��P@�C�@���@�n�@�ff@�^5@��#@��h@�`B@�?}@�7L@�/@�&�@�&�@�&�@���@��@��`@���@��9@��u@�A�@��@��F@�dZ@��@���@��!@�v�@�{@���@��h@���@���@�r�@�Z@�(�@��@��@��m@��;@��;@���@�ƨ@��@��@�l�@�S�@�C�@�33@�+@�o@�
=@�ȴ@���@�ff@�=q@�{@�@�@�7L@���@��j@��@�j@�Z@�I�@�9X@�1'@� �@�b@���@��
@��F@���@��@�dZ@�;d@��@��H@�{@���@�hs@�`B@�G�@�7L@�/@�/@�/@�&�@�&�@��@��@��@���@�Ĝ@�z�@��;@��P@�\)@�o@��@�^5@�=q@���@�@�hs@���@���@���@��@�1@\)@~ȴ@~@}�@}V@|�@{��@{33@z��@y��@x��@xA�@wl�@w
=@v�@v@u��@t�/@t(�@so@r�@qhs@p��@p�u@pr�@pQ�@p �@p  @o�P@nff@m��@m�@m�@m�@mV@l��@l�D@l�D@k�
@j��@ix�@i%@hĜ@h��@h1'@g�@g�P@f�y@e�@e`B@d��@d�j@d�D@dj@dI�@d9X@dI�@d9X@d9X@c��@c��@ct�@c"�@b�!@b�!@b^5@bM�@a��@a�@a�@a�^@a7L@`1'@_��@_��@_�@^��@^E�@^$�@]�@]�-@]�h@]`B@]`B@]?}@]?}@]�@\��@\1@[S�@Z��@Z�\@Zn�@Z^5@Z-@Y��@Y��@Yx�@YX@X��@X�u@XbN@W�;@W�@V�@V��@Vv�@VE�@V$�@V$�@V@V@V@U��@U�-@Up�@U�@UV@T�j@T�@Tz�@S��@Sƨ@S��@S��@S�@SdZ@SC�@So@R��@RM�@Q�@Q��@Qx�@Q�@P�`@P�9@Pr�@PA�@O�w@O|�@OK�@O�@N�y@N�R@N�+@N5?@M�h@M/@L�/@Lj@K�F@K�@K�@Kt�@KS�@KC�@J~�@I��@Ihs@I%@H  @Gl�@G\)@GK�@F�@E�@EO�@Dz�@CS�@C33@C@B��@B�\@Bn�@B�@A�^@Ax�@@��@@bN@@b@?��@?;d@?+@?�@?
=@>��@>�y@>�@>��@>v�@>5?@=�T@=p�@=/@<�/@<��@<�D@<j@<9X@;��@;�
@;dZ@;o@:�H@:��@:~�@:�@9��@9��@9x�@9hs@97L@9%@8�`@8��@8Q�@8  @7�;@7�;@7�;@7��@7��@7��@7�w@7�@7��@7l�@7
=@6�R@65?@5�@5��@5�@5/@5V@4��@4�/@4��@4��@4�D@49X@3��@3o@2�!@1��@1hs@1&�@1%@0��@0��@0�`@0��@0�9@0�u@0�u@0r�@0A�@/�@/+@.�@.�R@.$�@-�-@-��@-`B@-/@-V@-V@,��@,��@,�D@,z�@,j@,�@+�m@+��@+S�@+@*�!@*^5@)��@)�^@)X@(�`@(Ĝ@(�u@(  @'�;@'��@'�@'K�@';d@';d@'
=@&�R@&v�@&V@&E�@&5?@&{@%�T@%�h@%?}@$��@$��@$Z@#��@#t�@#S�@#33@#o@"�@"��@"n�@"=q@"J@!�^@!&�@ Ĝ@ �u@ Q�@ A�@ 1'@ 1'@  �@ b@   @�;@�@\)@
=@��@ȴ@�R@��@{@��@�-@��@�@V@�/@�j@�@j@(�@��@t�@33@�H@�!@�\@�@�#@��@��@X@&�@%@��@�@�@r�@1'@ �@b@��@\)@��@��@ff@5?@{@@��@��@��@��@��@�-@/@�@�j@I�@1@ƨ@ƨ@S�@�@��@��@M�@-@J@�^@��@��@X@��@�9@�u@r�@1'@b@b@�;@|�@\)@+@��@�@��@E�@{@��@�-@�@?}@�@V@��@�@�/@��@�D@j@I�@1@�
@��@t�@t�@dZ@33@o@
��@
~�@
n�@
M�@
�@
J@
J@
J@	�@	��@	�^@	��@	��@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB�BB�BE�BF�BG�BF�BI�BL�BN�BN�BO�BO�BO�BO�BZBe`Bn�Bs�Bw�B�=B�hB��B��B�3BĜB�B�B��BDB+B6FB49B7LB�B�BuBPB�`BǮB��B�9B�B��B��B��B�{B�uB�oB�hB�bB�bB�\B�\B�VB�\B�\B�PB�JB�B�By�BhsB_;BH�B:^B/B(�B"�B�B�B{BVBB�
B�}B�B��B��B�\B�+B~�Bx�Bn�BbNB\)BR�B?}B.BbB
�BB
�}B
�+B
cTB
S�B
1'B
�B
{B
oB
hB
hB
DB
1B	��B	��B	��B	�B	��B	�RB	��B	��B	�{B	�+B	z�B	hsB	.B	#�B	�B	�B	%B��B��B��B�sB�mB�`B�NB�#B��B��BƨBB�LB�!B��B��B��B��B��B��B�bB�VB�=B�1B�+B�%B�B�B�+B�%B�B�B�B|�B{�Bz�By�By�Bx�Bw�Bv�Bw�Bt�Br�Bp�Br�Bn�Bn�Bp�Bm�Bm�Bn�Bn�Bm�Bq�Bp�Bp�Bq�Br�Bs�Bt�Bt�B{�B|�Bz�Bv�Bx�Bx�Bu�Bw�B~�B}�B}�B|�B�B�B�B�B�B�B�B�%B�+B�=B�7B�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�'B�'B�'B�-B�9B�LB�dB�jB�qB�wB�wB��BȴB��B��B��B�B�;B�HB�BB�HB�fB�yB�B�B�B��B��B��B	B	+B	DB	VB	bB	hB	hB	oB	oB	uB	{B	{B	�B	�B	�B	�B	 �B	 �B	#�B	&�B	)�B	/B	/B	0!B	0!B	1'B	2-B	33B	49B	6FB	7LB	9XB	9XB	9XB	:^B	=qB	C�B	G�B	H�B	H�B	I�B	J�B	K�B	L�B	L�B	Q�B	VB	XB	[#B	\)B	_;B	dZB	e`B	gmB	gmB	iyB	l�B	l�B	m�B	n�B	r�B	v�B	w�B	y�B	{�B	}�B	}�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�9B	�FB	�RB	�XB	�dB	�jB	��B	ÖB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�BB	�HB	�HB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
"�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
.B
/B
/B
0!B
/B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
49B
5?B
5?B
5?B
6FB
7LB
7LB
9XB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
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
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
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
T�B
T�B
T�B
VB
T�B
VB
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
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
\)B
\)B
[#B
\)B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
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
m�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
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
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
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
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333BB�BB�BE�BF�BG�BF�BI�BL�BN�BN�BO�BO�BO�BO�BZBe`Bn�Bs�Bw�B�=B�hB��B��B�3BĜB�B�B��BDB+B6FB49B7LB�B�BuBPB�`BǮB��B�9B�B��B��B��B�{B�uB�oB�hB�bB�bB�\B�\B�VB�\B�\B�PB�JB�B�By�BhsB_;BH�B:^B/B(�B"�B�B�B{BVBB�
B�}B�B��B��B�\B�+B~�Bx�Bn�BbNB\)BR�B?}B.BbB
�BB
�}B
�+B
cTB
S�B
1'B
�B
{B
oB
hB
hB
DB
1B	��B	��B	��B	�B	��B	�RB	��B	��B	�{B	�+B	z�B	hsB	.B	#�B	�B	�B	%B��B��B��B�sB�mB�`B�NB�#B��B��BƨBB�LB�!B��B��B��B��B��B��B�bB�VB�=B�1B�+B�%B�B�B�+B�%B�B�B�B|�B{�Bz�By�By�Bx�Bw�Bv�Bw�Bt�Br�Bp�Br�Bn�Bn�Bp�Bm�Bm�Bn�Bn�Bm�Bq�Bp�Bp�Bq�Br�Bs�Bt�Bt�B{�B|�Bz�Bv�Bx�Bx�Bu�Bw�B~�B}�B}�B|�B�B�B�B�B�B�B�B�%B�+B�=B�7B�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�'B�'B�'B�-B�9B�LB�dB�jB�qB�wB�wB��BȴB��B��B��B�B�;B�HB�BB�HB�fB�yB�B�B�B��B��B��B	B	+B	DB	VB	bB	hB	hB	oB	oB	uB	{B	{B	�B	�B	�B	�B	 �B	 �B	#�B	&�B	)�B	/B	/B	0!B	0!B	1'B	2-B	33B	49B	6FB	7LB	9XB	9XB	9XB	:^B	=qB	C�B	G�B	H�B	H�B	I�B	J�B	K�B	L�B	L�B	Q�B	VB	XB	[#B	\)B	_;B	dZB	e`B	gmB	gmB	iyB	l�B	l�B	m�B	n�B	r�B	v�B	w�B	y�B	{�B	}�B	}�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�9B	�FB	�RB	�XB	�dB	�jB	��B	ÖB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�BB	�HB	�HB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
"�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
.B
/B
/B
0!B
/B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
49B
5?B
5?B
5?B
6FB
7LB
7LB
9XB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
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
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
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
T�B
T�B
T�B
VB
T�B
VB
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
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
\)B
\)B
[#B
\)B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
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
m�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
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
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
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
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230203095029  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230203005038  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230203005039  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230203005039                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230203005040  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230203005040  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230203010716                      G�O�G�O�G�O�                