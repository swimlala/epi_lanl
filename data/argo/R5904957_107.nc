CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:24Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140824  20181024140824  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               kA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�pT�1   @��eO��@3�/��w�c҇+J1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      kA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B ffBffBffBffB   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>�C@�CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\�C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cw�fCz  C{�fC}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D��D� DfD� D  D� D  D� D	  D	y�D
  D
� D  D� D��D� DfD� D  D� D  D� D  D� D  D� DfD� D��D� DfD�fD  D� D  D� D  D�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D�fDfD� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDGfDG� DH  DH� DI  DI� DI��DJ� DJ��DKy�DK��DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQy�DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV�fDW  DWy�DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dwy�Dw�3D�:=D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A��A�Q�A�Q�B�\B	�\B�\B�\B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba�\Bi�\Bq(�By(�B��{B�aHB��{B��{B��{B��{B��{B��{B��{B��{B��{B�ǮB��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=Cc�CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C0�C J=C"J=C$J=C&J=C(c�C*J=C,J=C.J=C0J=C2J=C4J=C60�C8J=C:J=C<J=C>c�C@c�CBJ=CDJ=CFJ=CHc�CJJ=CLJ=CNJ=CPJ=CRJ=CTc�CVJ=CXJ=CZJ=C\c�C^J=C`0�CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=Cpc�CrJ=CtJ=CvJ=Cx0�CzJ=C|0�C~0�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D�)D)D��D�D��D�D��D�D��D	�D	�)D
�D
��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!)D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA)DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ)DJ��DK)DK�)DL)DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ�)DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW�)DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�)Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr)Dr��Ds�Ds��Dt�Dt��Du�Du��Dv)Dv��Dw�Dw�)Dw��D�C�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�(�A�&�A�$�A��A�"�A��A��A��A��A�
=A���A���A��mA��
A���A��A�/A߸RA��TA��A��Aݺ^A�r�A�bNA��A�dZA��mA֕�A��`A�ZA���A��A��A�ĜA�p�A���A���A���A�-AǴ9A��HAĴ9A���A�x�A���A��/A�\)A�
=A�A�;dA�t�A�G�A�{A��PA��A�ffA�%A�Q�A�A�A��9A�%A�A���A��A���A���A���A�bNA�ƨA�+A���A�(�A��A���A���A�=qA�%A�XA��HA���A�M�A�^5A�9XA��A�+A�XA��yA�-A��!A�(�A�&�A���A�=qA��A��/A�A�p�A��wA�7LA��;A~��A|�A{��AzffAx�`Aw�;Av�jAu/Ar~�Ap-Am\)Aj5?Af��Ac&�Aa��A`Q�A]t�A[7LAZ��AY��AWAU�-AT��AS��AS%AQ�wANz�AL�jAJ�HAJI�AI�PAH��AG�AG��AG\)AF��AFz�AC"�AA33A?`BA>5?A=;dA;�#A9C�A6�A6A�A5��A4��A3K�A2-A0��A0$�A-��A,v�A+�#A(�DA'A'\)A&�A$��A#O�A"�/A"{A ��A �A`BA7LA��A�
A��A��A��A�uAAƨA��A�A��A7LA9XA��AbAC�A �A&�A�AVA
��AbNAVAbNA��AXAZA��A��A�+A$�AhsA1'AƨAXA �A �A �\A 1@���@���@���@��@�~�@��;@܃@�J@�7L@�o@ӕ�@���@�
=@ӥ�@��@�j@ԃ@���@��/@���@ԣ�@�I�@�(�@Ӆ@��H@�ff@���@�p�@���@�;d@ɉ7@��
@Ǿw@ǥ�@Ǯ@���@�J@�G�@�&�@��/@ÍP@�|�@�A�@�&�@ũ�@�x�@Ĭ@���@���@�%@���@���@�|�@���@�E�@�{@��#@�@���@�hs@���@�bN@���@�
=@�"�@��H@��\@�$�@�/@���@�bN@���@���@��
@��H@�M�@�$�@���@��@��T@�@�?}@��@�ƨ@��P@�dZ@�S�@���@�?}@��`@���@��9@��@���@�Z@�dZ@�V@���@���@��u@�j@�9X@�(�@��m@��@�S�@���@��H@�ȴ@���@�~�@�V@�M�@�=q@��@���@��h@��7@�x�@��@���@�(�@��
@���@�|�@�K�@��@���@��@���@�ȴ@�ȴ@��!@���@��!@��R@���@���@���@�^5@��@���@��j@��j@��j@�Ĝ@�Ĝ@��j@��D@���@��u@���@���@���@��u@���@�j@�  @��@��m@��
@��@�K�@��y@�v�@���@�X@�&�@��j@�r�@�b@�b@���@�dZ@�o@�n�@���@���@��h@�p�@�X@�`B@�hs@�hs@�O�@���@�r�@� �@��@�C�@��\@�=q@�{@���@���@��h@���@�G�@���@�Q�@���@���@�|�@�dZ@�K�@���@��!@��\@�M�@�5?@�{@��@���@���@�x�@�hs@�`B@�`B@�X@�X@�X@�&�@��j@���@�bN@�1'@���@�;d@�33@�o@�ȴ@���@���@�^5@�J@��h@��@���@�(�@��;@��@��@�;d@�ȴ@�5?@���@�O�@���@��/@��D@��m@��@�dZ@�@�M�@�J@��T@���@���@�G�@���@�Ĝ@��D@��@��@u�@f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�"�A�(�A�&�A�$�A��A�"�A��A��A��A��A�
=A���A���A��mA��
A���A��A�/A߸RA��TA��A��Aݺ^A�r�A�bNA��A�dZA��mA֕�A��`A�ZA���A��A��A�ĜA�p�A���A���A���A�-AǴ9A��HAĴ9A���A�x�A���A��/A�\)A�
=A�A�;dA�t�A�G�A�{A��PA��A�ffA�%A�Q�A�A�A��9A�%A�A���A��A���A���A���A�bNA�ƨA�+A���A�(�A��A���A���A�=qA�%A�XA��HA���A�M�A�^5A�9XA��A�+A�XA��yA�-A��!A�(�A�&�A���A�=qA��A��/A�A�p�A��wA�7LA��;A~��A|�A{��AzffAx�`Aw�;Av�jAu/Ar~�Ap-Am\)Aj5?Af��Ac&�Aa��A`Q�A]t�A[7LAZ��AY��AWAU�-AT��AS��AS%AQ�wANz�AL�jAJ�HAJI�AI�PAH��AG�AG��AG\)AF��AFz�AC"�AA33A?`BA>5?A=;dA;�#A9C�A6�A6A�A5��A4��A3K�A2-A0��A0$�A-��A,v�A+�#A(�DA'A'\)A&�A$��A#O�A"�/A"{A ��A �A`BA7LA��A�
A��A��A��A�uAAƨA��A�A��A7LA9XA��AbAC�A �A&�A�AVA
��AbNAVAbNA��AXAZA��A��A�+A$�AhsA1'AƨAXA �A �A �\A 1@���@���@���@��@�~�@��;@܃@�J@�7L@�o@ӕ�@���@�
=@ӥ�@��@�j@ԃ@���@��/@���@ԣ�@�I�@�(�@Ӆ@��H@�ff@���@�p�@���@�;d@ɉ7@��
@Ǿw@ǥ�@Ǯ@���@�J@�G�@�&�@��/@ÍP@�|�@�A�@�&�@ũ�@�x�@Ĭ@���@���@�%@���@���@�|�@���@�E�@�{@��#@�@���@�hs@���@�bN@���@�
=@�"�@��H@��\@�$�@�/@���@�bN@���@���@��
@��H@�M�@�$�@���@��@��T@�@�?}@��@�ƨ@��P@�dZ@�S�@���@�?}@��`@���@��9@��@���@�Z@�dZ@�V@���@���@��u@�j@�9X@�(�@��m@��@�S�@���@��H@�ȴ@���@�~�@�V@�M�@�=q@��@���@��h@��7@�x�@��@���@�(�@��
@���@�|�@�K�@��@���@��@���@�ȴ@�ȴ@��!@���@��!@��R@���@���@���@�^5@��@���@��j@��j@��j@�Ĝ@�Ĝ@��j@��D@���@��u@���@���@���@��u@���@�j@�  @��@��m@��
@��@�K�@��y@�v�@���@�X@�&�@��j@�r�@�b@�b@���@�dZ@�o@�n�@���@���@��h@�p�@�X@�`B@�hs@�hs@�O�@���@�r�@� �@��@�C�@��\@�=q@�{@���@���@��h@���@�G�@���@�Q�@���@���@�|�@�dZ@�K�@���@��!@��\@�M�@�5?@�{@��@���@���@�x�@�hs@�`B@�`B@�X@�X@�X@�&�@��j@���@�bN@�1'@���@�;d@�33@�o@�ȴ@���@���@�^5@�J@��h@��@���@�(�@��;@��@��@�;d@�ȴ@�5?@���@�O�@���@��/@��D@��m@��@�dZ@�@�M�@�J@��T@���@���@�G�@���@�Ĝ@��D@��@��@u�@f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B(�B7LBG�BE�BE�BD�BA�B<jB8RB9XBB�BYBw�B�DB�PB��B��B��B��B�dB��B�#B�mB�B�B��B��BDBoB�B#�B#�B2-B.B)�B-B-B.BB�BaHBu�B�B~�Bw�Br�B>wB�B�wB�!B��B��B��B�\B�+B� Bz�By�Bx�Bv�Br�Bo�Bk�BiyB�1B�-B�JBl�B�VBƨB�dB�B�{B�=Bt�BcTBYB49BVB
�B
�wB
�oB
s�B
^5B
K�B
6FB
&�B
 �B
�B
JB
B	��B	�B	�HB	��B	ÖB	�!B	��B	�+B	|�B	r�B	bNB	W
B	R�B	K�B	<jB	0!B	>wB	@�B	:^B	1'B	!�B	�B	VB	DB	+B	B	  B	  B��B��B��B�mB�;B�B��B��BǮB��B�jB�XB�LB�9B�'B�!B�'B�9B�B��B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B��B�oB�uB��B��B��B�{B��B��B��B�hB�oB�uB�uB�\B�B|�Bw�Bv�Bz�B|�B~�B�+B��B��B�oB�DB�7B�1B�1B�JB�VB�PB�PB�uB��B��B��BF�B��B��B��B��B�B�3B�FB�qB��BÖBÖBȴB��B��B��B��B��BɺBǮBŢBÖBB��B�qB�jB�wB�wB�}B��BƨBȴB��B��B�B�/B�;B�ZB�yB�B�B�B��B��B��B��B��B	B	%B	B	%B	1B		7B	
=B	JB	hB	uB	�B	�B	#�B	2-B	6FB	6FB	9XB	;dB	C�B	J�B	L�B	N�B	T�B	YB	[#B	\)B	]/B	\)B	]/B	`BB	e`B	k�B	m�B	o�B	o�B	s�B	v�B	w�B	w�B	x�B	z�B	{�B	z�B	z�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�hB	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�3B	�3B	�?B	�FB	�LB	�jB	ÖB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�BB	�;B	�;B	�;B	�;B	�;B	�BB	�NB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
DB
DB

=B

=B
	7B

=B
DB
DB
DB
JB
bB
%,B
3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B(�B7LBG�BE�BE�BD�BA�B<jB8RB9XBB�BYBw�B�DB�PB��B��B��B��B�dB��B�#B�mB�B�B��B��BDBoB�B#�B#�B2-B.B)�B-B-B.BB�BaHBu�B�B~�Bw�Br�B>wB�B�wB�!B��B��B��B�\B�+B� Bz�By�Bx�Bv�Br�Bo�Bk�BiyB�1B�-B�JBl�B�VBƨB�dB�B�{B�=Bt�BcTBYB49BVB
�B
�wB
�oB
s�B
^5B
K�B
6FB
&�B
 �B
�B
JB
B	��B	�B	�HB	��B	ÖB	�!B	��B	�+B	|�B	r�B	bNB	W
B	R�B	K�B	<jB	0!B	>wB	@�B	:^B	1'B	!�B	�B	VB	DB	+B	B	  B	  B��B��B��B�mB�;B�B��B��BǮB��B�jB�XB�LB�9B�'B�!B�'B�9B�B��B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B��B�oB�uB��B��B��B�{B��B��B��B�hB�oB�uB�uB�\B�B|�Bw�Bv�Bz�B|�B~�B�+B��B��B�oB�DB�7B�1B�1B�JB�VB�PB�PB�uB��B��B��BF�B��B��B��B��B�B�3B�FB�qB��BÖBÖBȴB��B��B��B��B��BɺBǮBŢBÖBB��B�qB�jB�wB�wB�}B��BƨBȴB��B��B�B�/B�;B�ZB�yB�B�B�B��B��B��B��B��B	B	%B	B	%B	1B		7B	
=B	JB	hB	uB	�B	�B	#�B	2-B	6FB	6FB	9XB	;dB	C�B	J�B	L�B	N�B	T�B	YB	[#B	\)B	]/B	\)B	]/B	`BB	e`B	k�B	m�B	o�B	o�B	s�B	v�B	w�B	w�B	x�B	z�B	{�B	z�B	z�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�hB	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�3B	�3B	�?B	�FB	�LB	�jB	ÖB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�BB	�;B	�;B	�;B	�;B	�;B	�BB	�NB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
DB
DB

=B

=B
	7B

=B
DB
DB
DB
JB
bB
%,B
3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140824                              AO  ARCAADJP                                                                    20181024140824    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140824  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140824  QCF$                G�O�G�O�G�O�4000            