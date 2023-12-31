CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2019-09-25T06:44:15Z creation;2019-09-25T06:44:18Z conversion to V3.1      
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
resolution        =���   axis      Z        D  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ʘ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190925064415  20190925065717  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA                                  2B  A   APEX                            7906                            051216                          846 @��F��1   @��GI��J@1��1'�en��P1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�33B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D��D� D  D� D  D�fDfD� D  D� D  D� D��D� D  D� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_�fD`fD`�fDa  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��3D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʃ3D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D΀ Dμ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԼ�D���D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�3D�C3D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D�3D�� D���D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A$(�AB�\Ab�\A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B(��B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bip�Bq
=By
=B��B��B��RB��RB��B��B��RB��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�C(�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6\)C8\)C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CV\)CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�Ch(�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~\)C�!HC�!HC�!HC�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D�>D
>D��D�D��D�D�
D
D��D�D��D�D��D
>D��D�D��D�D��D
D��D�D��D�D�>D�D��D�D��D�D��D�D��D�D��D�D��D �D �>D!�D!��D"�D"��D#�D#��D$�D$�
D%
D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\
D\��D]�D]��D^�D^��D_�D_�
D`
D`�
Da�Da��Db�Db��Dc
>Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�>Dl�Dl��Dm�Dm��Dn�Dn�
Do�Do��Dp�Dp��Dq�Dq��Dr
>Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�RD�HRD��RD��RD�D�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��D�RD�HRD��RD��RD�RD�HRD��RD��D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�K�D��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�D�HRD��RD��RD�RD�HRD��RD��D�D�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD���D��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD��D�HRD��RD�˅D�RD�HRD��D��D�RD�HRD��RD��RD�RD�HRD��RD��D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD�˅D��D�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD�˅D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRDRD��RD�RD�HRDÈRD��RD�RD�HRDĈRD��RD�RD�HRDňRD��RD�RD�HRDƈRD��RD�RD�HRDǋ�D��RD�RD�HRDȈRD��RD�RD�HRDɈRD��RD�RD�HRDʋ�D�˅D�RD�HRDˈRD��RD�RD�HRD̈RD��RD�RD�HRDͅD��RD�RD�HRDΈRD��D�RD�HRDψRD��RD�RD�HRDЈRD��RD�RD�HRDшRD�˅D�RD�HRD҈RD��RD�RD�HRDӈRD��RD�RD�HRDԈRD��D�D�EDՅD��RD�RD�HRDֈRD��RD�RD�HRD׈RD��RD�RD�HRD؈RD��RD�RD�HRDوRD��RD�RD�HRDڈRD��RD�RD�HRDۅD��RD�RD�HRD܈RD��RD�RD�HRD݈RD��RD�RD�HRDވRD��RD��D�K�D߈RD��RD�RD�ED��RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��D�D�HRD担D��RD�D�HRD�RD��RD�RD�K�D�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��D�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD��RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD�˅D�RD�HRD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA�n�A�5?A��A�%A���A��A��A��`A��;A��A���A���A�ĜA�jA�A曦A�+A�v�A�p�A�l�A�ffA�dZA�ZA�S�A�33A�A�!A�7A�5?A���A䕁A�5?A�"�A�O�A�v�A���A���A�A��TA�C�A�ƨA�+A�M�A��HA�~�AĶFA�n�A��A��A�ƨAÏ\A�VA7A�A�-A��
A���A��/A��A��A���A��A���A���A�bNA��#A���A�JA���A��-A��A�G�A�r�A�E�A���A��yA�p�A���A�z�A�Q�A�9XA�E�A�ĜA�/A���A�7LA�VA��A�K�A� �A�/A�VA�S�A��!A�`BA�ZA���A��;A�/A��wA�~�A�dZA��RA��A���A�^5A��A���A��`A�XA��A��
A{��Az(�Ay�^AyC�Ax��Ax��Ax~�Aw�PAu�An��Aj(�Ae��Ad�+AcS�Ab(�A^-AY\)AUhsAP�AK��AJI�AJJAIƨAIC�AH��AH1'AG�AGAF$�AEhsAD��AD9XAB��AAK�A@��A@v�A?�FA>I�A<�DA8jA57LA4bNA2��A29XA1dZA.�A,VA*��A*VA)��A)��A)hsA)"�A(ĜA(n�A%�TA#oA!��A 5?A�A�-A��A/AA�A�uA��A�AC�A�A��A=qA��Ax�AhsA`BA/A��A�^A\)A�HAbNA�PA�AA�A��A�
A�^A
��A	��A��AbA�A
=At�AĜAn�AbAC�@�ƨ@�v�@�C�@��@�/@�V@���@��9@�z�@�t�@��R@���@�7L@�J@�@�
=@�@�z�@�+@��@�C�@�C�@�!@���@�r�@��@�\)@�M�@�hs@��/@�j@�u@�|�@�v�@�-@�@�9X@��@ߥ�@ݡ�@���@ܬ@�Q�@� �@���@�t�@�@�M�@���@��@���@֏\@�V@���@�`B@�G�@Ԭ@�I�@� �@��@ӍP@��@ҧ�@���@�~�@�M�@ѡ�@���@�\)@ΰ!@�v�@�M�@�=q@�$�@�@͡�@�`B@�X@�`B@�O�@�Ĝ@�r�@�b@�ƨ@˅@�o@�ff@��`@ȃ@�j@�A�@Ǿw@�"�@�@��H@��@��@�
=@��@�?}@�b@�^5@��@���@��h@�?}@�Q�@�|�@���@�@�
=@�v�@��^@�G�@���@�z�@�A�@�(�@�  @��@�"�@�^5@��@��7@���@�1@��@�|�@��y@��R@���@�v�@�M�@�5?@�-@�{@��@�O�@�7L@�%@��@�Ĝ@��u@�j@� �@���@�;d@�
=@��@���@���@�~�@�M�@�E�@�5?@�5?@�-@��@�`B@���@��9@��D@�z�@��;@�t�@��@��R@�E�@��@��7@�&�@��@���@���@�1@��@�=q@���@���@�`B@�O�@�7L@���@���@���@��@�S�@�;d@��@��H@�ȴ@���@���@�ff@��#@���@��D@�j@�A�@�1'@�(�@���@��
@��@�K�@��@�V@�@���@���@��h@��7@��@�X@�V@���@�I�@�b@��@�ƨ@�l�@�+@���@�J@�@���@�hs@��@���@��9@��m@��@���@�S�@�+@�
=@���@�n�@�@��`@��9@���@�  @�\)@���@�n�@�E�@�J@���@���@�O�@��@��D@�r�@�Z@��@���@��!@���@�-@���@�@���@�p�@���@���@��@�bN@�I�@�1'@�(�@���@��@��@���@�n�@�-@��@��-@��7@�G�@�?}@�&�@���@��@��u@��@�r�@�I�@�  @�;d@���@���@�n�@�=q@���@�hs@�7L@��`@��@��D@�r�@�bN@�  @�\)@�33@��@�@��y@���@��!@���@�M�@�{@��@�@���@��h@�x�@�hs@�O�@�G�@�?}@�7L@�&�@���@��`@��9@�bN@��m@�C�@�
=@��R@�5?@���@��@�r�@�A�@�@+@~E�@|Z@z��@z=q@y��@y&�@xQ�@w��@w|�@wl�@wl�@w;d@w+@v��@vff@tI�@s�@sC�@r�!@r~�@rM�@q�7@pĜ@pb@o��@o��@oK�@n�y@nv�@n@m��@m`B@m?}@m/@m�@l��@l�j@l�j@l��@k�m@j�H@i�#@hbN@g��@g
=@f�y@f�R@f�+@fff@f$�@f@e��@e��@e�-@e�h@eV@dj@d9X@c�m@c@b�!@bn�@b-@b�@a��@a�#@a��@`��@_�@`  @_|�@_;d@_�@^�@^��@^v�@^5?@^{@]�-@]O�@\��@\Z@\�@[��@[t�@[C�@["�@["�@["�@Z�@Z�\@Y�@Y�@X�@XbN@XbN@XQ�@XA�@XA�@XA�@X �@W�;@W\)@V�y@V$�@U��@UV@TI�@S�m@S��@SdZ@SS�@SC�@SS�@SdZ@S"�@R�\@R��@R=q@Q��@Q�7@Q%@O�;@O�P@N�+@L��@K�@KdZ@KC�@J�H@J�H@J��@J��@J��@J��@J�!@J��@J�\@J^5@I��@I��@I�7@I&�@HĜ@H�u@Hr�@G��@F�@F��@F��@Fv�@Fff@Fff@Fff@FV@F5?@F$�@EO�@D�@D��@D�@D�D@D�D@Dz�@Dj@Dj@C��@C�F@B~�@AG�@@�u@@�u@@�u@@ �@?��@?�@?�@?�w@?�@?��@?��@?��@?��@?�@?��@?l�@>ȴ@>��@>��@>v�@>E�@>5?@=�@=�-@=�-@=p�@=p�@<��@<j@<(�@<1@;��@;S�@;dZ@;dZ@;S�@:�H@:��@:M�@9��@97L@8�9@8r�@8 �@7�;@7�w@7K�@6�R@6�+@6v�@6ff@6ff@6V@6E�@6E�@65?@6$�@6{@6{@6{@5�@5�@5@5`B@5�@4�/@4�/@4��@4�j@4z�@4�D@4�@3ƨ@3C�@3o@2��@2^5@1��@1hs@1&�@0��@0�u@0bN@/|�@/
=@.�@.�R@.v�@.V@.5?@.@-�@-�T@-�h@-V@,��@+�m@+��@+o@*�@*�H@*�H@*�H@*��@*��@*~�@*^5@*�@)�#@)�7@)x�@)hs@(��@(�`@(Ĝ@'�@'�@'|�@';d@'�@&�@%�@%��@%�T@%�T@%�T@%@%�-@%?}@$�@$�@$��@$�D@$Z@$9X@#�F@"�!@"~�@!��@!�^@!�^@!�^@!�^@!��@!%@ Ĝ@ �9@  �@�w@|�@+@+@�@�y@�R@�+@{@�-@��@p�@�@�j@Z@�@��@�
@��@t�@C�@C�@o@o@�@�@�H@��@��@��@�H@��@��@�!@n�@M�@-@�#@�#@��@��@��@X@7L@&�@%@7L@hs@Ĝ@r�@1'@b@�;@�@�P@K�@;d@+@
=@ȴ@��@��@v�@ff@$�@{@{@$�@{@��@p�@`B@O�@O�@O�@?}@�@V@��@��@�@��@��@�@�/@��@�j@j@1@�F@�F@�F@��@t�@t�@t�@t�@S�@o@o@@�@��@�!@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA�n�A�5?A��A�%A���A��A��A��`A��;A��A���A���A�ĜA�jA�A曦A�+A�v�A�p�A�l�A�ffA�dZA�ZA�S�A�33A�A�!A�7A�5?A���A䕁A�5?A�"�A�O�A�v�A���A���A�A��TA�C�A�ƨA�+A�M�A��HA�~�AĶFA�n�A��A��A�ƨAÏ\A�VA7A�A�-A��
A���A��/A��A��A���A��A���A���A�bNA��#A���A�JA���A��-A��A�G�A�r�A�E�A���A��yA�p�A���A�z�A�Q�A�9XA�E�A�ĜA�/A���A�7LA�VA��A�K�A� �A�/A�VA�S�A��!A�`BA�ZA���A��;A�/A��wA�~�A�dZA��RA��A���A�^5A��A���A��`A�XA��A��
A{��Az(�Ay�^AyC�Ax��Ax��Ax~�Aw�PAu�An��Aj(�Ae��Ad�+AcS�Ab(�A^-AY\)AUhsAP�AK��AJI�AJJAIƨAIC�AH��AH1'AG�AGAF$�AEhsAD��AD9XAB��AAK�A@��A@v�A?�FA>I�A<�DA8jA57LA4bNA2��A29XA1dZA.�A,VA*��A*VA)��A)��A)hsA)"�A(ĜA(n�A%�TA#oA!��A 5?A�A�-A��A/AA�A�uA��A�AC�A�A��A=qA��Ax�AhsA`BA/A��A�^A\)A�HAbNA�PA�AA�A��A�
A�^A
��A	��A��AbA�A
=At�AĜAn�AbAC�@�ƨ@�v�@�C�@��@�/@�V@���@��9@�z�@�t�@��R@���@�7L@�J@�@�
=@�@�z�@�+@��@�C�@�C�@�!@���@�r�@��@�\)@�M�@�hs@��/@�j@�u@�|�@�v�@�-@�@�9X@��@ߥ�@ݡ�@���@ܬ@�Q�@� �@���@�t�@�@�M�@���@��@���@֏\@�V@���@�`B@�G�@Ԭ@�I�@� �@��@ӍP@��@ҧ�@���@�~�@�M�@ѡ�@���@�\)@ΰ!@�v�@�M�@�=q@�$�@�@͡�@�`B@�X@�`B@�O�@�Ĝ@�r�@�b@�ƨ@˅@�o@�ff@��`@ȃ@�j@�A�@Ǿw@�"�@�@��H@��@��@�
=@��@�?}@�b@�^5@��@���@��h@�?}@�Q�@�|�@���@�@�
=@�v�@��^@�G�@���@�z�@�A�@�(�@�  @��@�"�@�^5@��@��7@���@�1@��@�|�@��y@��R@���@�v�@�M�@�5?@�-@�{@��@�O�@�7L@�%@��@�Ĝ@��u@�j@� �@���@�;d@�
=@��@���@���@�~�@�M�@�E�@�5?@�5?@�-@��@�`B@���@��9@��D@�z�@��;@�t�@��@��R@�E�@��@��7@�&�@��@���@���@�1@��@�=q@���@���@�`B@�O�@�7L@���@���@���@��@�S�@�;d@��@��H@�ȴ@���@���@�ff@��#@���@��D@�j@�A�@�1'@�(�@���@��
@��@�K�@��@�V@�@���@���@��h@��7@��@�X@�V@���@�I�@�b@��@�ƨ@�l�@�+@���@�J@�@���@�hs@��@���@��9@��m@��@���@�S�@�+@�
=@���@�n�@�@��`@��9@���@�  @�\)@���@�n�@�E�@�J@���@���@�O�@��@��D@�r�@�Z@��@���@��!@���@�-@���@�@���@�p�@���@���@��@�bN@�I�@�1'@�(�@���@��@��@���@�n�@�-@��@��-@��7@�G�@�?}@�&�@���@��@��u@��@�r�@�I�@�  @�;d@���@���@�n�@�=q@���@�hs@�7L@��`@��@��D@�r�@�bN@�  @�\)@�33@��@�@��y@���@��!@���@�M�@�{@��@�@���@��h@�x�@�hs@�O�@�G�@�?}@�7L@�&�@���@��`@��9@�bN@��m@�C�@�
=@��R@�5?@���@��@�r�@�A�@�@+@~E�@|Z@z��@z=q@y��@y&�@xQ�@w��@w|�@wl�@wl�@w;d@w+@v��@vff@tI�@s�@sC�@r�!@r~�@rM�@q�7@pĜ@pb@o��@o��@oK�@n�y@nv�@n@m��@m`B@m?}@m/@m�@l��@l�j@l�j@l��@k�m@j�H@i�#@hbN@g��@g
=@f�y@f�R@f�+@fff@f$�@f@e��@e��@e�-@e�h@eV@dj@d9X@c�m@c@b�!@bn�@b-@b�@a��@a�#@a��@`��@_�@`  @_|�@_;d@_�@^�@^��@^v�@^5?@^{@]�-@]O�@\��@\Z@\�@[��@[t�@[C�@["�@["�@["�@Z�@Z�\@Y�@Y�@X�@XbN@XbN@XQ�@XA�@XA�@XA�@X �@W�;@W\)@V�y@V$�@U��@UV@TI�@S�m@S��@SdZ@SS�@SC�@SS�@SdZ@S"�@R�\@R��@R=q@Q��@Q�7@Q%@O�;@O�P@N�+@L��@K�@KdZ@KC�@J�H@J�H@J��@J��@J��@J��@J�!@J��@J�\@J^5@I��@I��@I�7@I&�@HĜ@H�u@Hr�@G��@F�@F��@F��@Fv�@Fff@Fff@Fff@FV@F5?@F$�@EO�@D�@D��@D�@D�D@D�D@Dz�@Dj@Dj@C��@C�F@B~�@AG�@@�u@@�u@@�u@@ �@?��@?�@?�@?�w@?�@?��@?��@?��@?��@?�@?��@?l�@>ȴ@>��@>��@>v�@>E�@>5?@=�@=�-@=�-@=p�@=p�@<��@<j@<(�@<1@;��@;S�@;dZ@;dZ@;S�@:�H@:��@:M�@9��@97L@8�9@8r�@8 �@7�;@7�w@7K�@6�R@6�+@6v�@6ff@6ff@6V@6E�@6E�@65?@6$�@6{@6{@6{@5�@5�@5@5`B@5�@4�/@4�/@4��@4�j@4z�@4�D@4�@3ƨ@3C�@3o@2��@2^5@1��@1hs@1&�@0��@0�u@0bN@/|�@/
=@.�@.�R@.v�@.V@.5?@.@-�@-�T@-�h@-V@,��@+�m@+��@+o@*�@*�H@*�H@*�H@*��@*��@*~�@*^5@*�@)�#@)�7@)x�@)hs@(��@(�`@(Ĝ@'�@'�@'|�@';d@'�@&�@%�@%��@%�T@%�T@%�T@%@%�-@%?}@$�@$�@$��@$�D@$Z@$9X@#�F@"�!@"~�@!��@!�^@!�^@!�^@!�^@!��@!%@ Ĝ@ �9@  �@�w@|�@+@+@�@�y@�R@�+@{@�-@��@p�@�@�j@Z@�@��@�
@��@t�@C�@C�@o@o@�@�@�H@��@��@��@�H@��@��@�!@n�@M�@-@�#@�#@��@��@��@X@7L@&�@%@7L@hs@Ĝ@r�@1'@b@�;@�@�P@K�@;d@+@
=@ȴ@��@��@v�@ff@$�@{@{@$�@{@��@p�@`B@O�@O�@O�@?}@�@V@��@��@�@��@��@�@�/@��@�j@j@1@�F@�F@�F@��@t�@t�@t�@t�@S�@o@o@@�@��@�!@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�wB	�qB	�jB	�jB	�jB	�jB	�jB	�dB	�dB	�dB	�jB	�dB	�dB	�dB	�^B	�XB	�LB	�FB	�9B	�3B	�9B	�3B	�9B	�3B	�-B	�!B	�B	��B	��B	��B	�hB	�PB	�PB	��B	��B	�mB
B
$�B
,B
7LB
P�B
XB
ZB
\)B
dZB
k�B
r�B
s�B
v�B
x�B
y�B
|�B
� B
�DB
�bB
��B
�9B
�wB
ŢB
��B
��B
�B
�B
��B{B�B%�BG�BT�B\)Be`Bl�Bu�B{�B�DB�FB��B�NB�mB�B�TB�fB��B�?B�B��B��B�{B�7B� Bn�Bk�B\)BT�BS�BT�BN�BE�B1'B'�B#�B�B+B
��B
�BB
��B
��B
ŢB
�dB
��B
y�B
7LB
�B	��B	�B	�B	�B	�sB	�mB	�ZB	�5B	��B	�9B	��B	�%B	|�B	v�B	m�B	^5B	O�B	7LB	,B	�B	\B	VB	VB	JB	DB		7B	+B	B	B	B	  B��B��B��B��B��B��B�B�B�B�fB�ZB�TB�NB�HB�NB�TB�NB�NB�TB�ZB�`B�`B�`B�ZB�yB�yB�yB�B�yB�sB�B�sB�fB�fB�`B�`B�ZB�`B�`B�`B�`B�`B�ZB�ZB�ZB�fB�sB�B�B��B�B�B�B�B�B�B�B�B�yB�mB�B�B�B�B�yB�sB�B�B�B��B��B	  B	B	B	B	B	B	B	B	B		7B	PB	\B	VB	\B	\B	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	)�B	,B	.B	1'B	2-B	2-B	2-B	8RB	:^B	:^B	;dB	<jB	<jB	>wB	?}B	@�B	F�B	I�B	N�B	N�B	O�B	Q�B	R�B	R�B	S�B	T�B	T�B	T�B	W
B	]/B	aHB	e`B	hsB	k�B	p�B	t�B	r�B	s�B	t�B	t�B	t�B	u�B	v�B	w�B	x�B	{�B	}�B	�B	�B	�B	�B	�+B	�=B	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�!B	�!B	�'B	�FB	�jB	�jB	�^B	�qB	�wB	��B	ÖB	ÖB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�HB	�NB	�TB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B
JB
PB
PB
PB
PB
VB
VB
VB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
uB
oB
uB
uB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
+B
+B
,B
,B
,B
-B
-B
-B
/B
0!B
0!B
0!B
0!B
1'B
2-B
1'B
1'B
1'B
2-B
1'B
1'B
2-B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
L�B
M�B
N�B
O�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
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
XB
XB
YB
XB
XB
XB
XB
YB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
bNB
bNB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
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
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
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
m�B
m�B
m�B
m�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
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
z�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
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
� B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�wB	�qB	�jB	�jB	�jB	�jB	�jB	�dB	�dB	�dB	�jB	�dB	�dB	�dB	�^B	�XB	�LB	�FB	�9B	�3B	�9B	�3B	�9B	�3B	�-B	�!B	�B	��B	��B	��B	�hB	�PB	�PB	��B	��B	�mB
B
$�B
,B
7LB
P�B
XB
ZB
\)B
dZB
k�B
r�B
s�B
v�B
x�B
y�B
|�B
� B
�DB
�bB
��B
�9B
�wB
ŢB
��B
��B
�B
�B
��B{B�B%�BG�BT�B\)Be`Bl�Bu�B{�B�DB�FB��B�NB�mB�B�TB�fB��B�?B�B��B��B�{B�7B� Bn�Bk�B\)BT�BS�BT�BN�BE�B1'B'�B#�B�B+B
��B
�BB
��B
��B
ŢB
�dB
��B
y�B
7LB
�B	��B	�B	�B	�B	�sB	�mB	�ZB	�5B	��B	�9B	��B	�%B	|�B	v�B	m�B	^5B	O�B	7LB	,B	�B	\B	VB	VB	JB	DB		7B	+B	B	B	B	  B��B��B��B��B��B��B�B�B�B�fB�ZB�TB�NB�HB�NB�TB�NB�NB�TB�ZB�`B�`B�`B�ZB�yB�yB�yB�B�yB�sB�B�sB�fB�fB�`B�`B�ZB�`B�`B�`B�`B�`B�ZB�ZB�ZB�fB�sB�B�B��B�B�B�B�B�B�B�B�B�yB�mB�B�B�B�B�yB�sB�B�B�B��B��B	  B	B	B	B	B	B	B	B	B		7B	PB	\B	VB	\B	\B	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	)�B	,B	.B	1'B	2-B	2-B	2-B	8RB	:^B	:^B	;dB	<jB	<jB	>wB	?}B	@�B	F�B	I�B	N�B	N�B	O�B	Q�B	R�B	R�B	S�B	T�B	T�B	T�B	W
B	]/B	aHB	e`B	hsB	k�B	p�B	t�B	r�B	s�B	t�B	t�B	t�B	u�B	v�B	w�B	x�B	{�B	}�B	�B	�B	�B	�B	�+B	�=B	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�!B	�!B	�'B	�FB	�jB	�jB	�^B	�qB	�wB	��B	ÖB	ÖB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�HB	�NB	�TB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B
JB
PB
PB
PB
PB
VB
VB
VB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
uB
oB
uB
uB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
+B
+B
,B
,B
,B
-B
-B
-B
/B
0!B
0!B
0!B
0!B
1'B
2-B
1'B
1'B
1'B
2-B
1'B
1'B
2-B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
L�B
M�B
N�B
O�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
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
XB
XB
YB
XB
XB
XB
XB
YB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
bNB
bNB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
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
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
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
m�B
m�B
m�B
m�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
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
z�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
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
� B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20190925154333  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190925064415  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190925064416  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190925064416  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190925064417  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190925064417  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190925064417  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190925064417  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190925064417  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190925064418                      G�O�G�O�G�O�                JA  ARUP                                                                        20190925065717                      G�O�G�O�G�O�                