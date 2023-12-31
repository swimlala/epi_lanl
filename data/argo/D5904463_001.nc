CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:44Z AOML 3.0 creation; 2016-08-07T22:44:32Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ``   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܐ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߐ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20150226221444  20160807154432  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_001                   2C  D   APEX                            6530                            072314                          846 @�t 0� 1   @�t��@*��vȴ�cs�l�C�1   GPS     Primary sampling: averaged [2dbar-bin averaged]                                                                                                                                                                                                                    A   A   D   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B'��B0��B7��B@  BH  BP  BX  Bb  BfffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=Bp�B
=B!
=B(��B1�
B8��BA
=BI
=BQ
=BY
=Bc
=Bgp�Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B�Q�C (�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��D��D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��D��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRDRD��RD�RD�HRDÈRD��RD�RD�HRDĈRD��RD�RD�HRDňRD��RD�RD�HRDƈRD��RD�RD�HRDǈRD��RD�RD�HRDȈRD��RD�RD�HRDɈRD��RD�RD�HRDʈRD��RD�RD�HRDˈRD��RD�RD�HRD̈RD��RD�RD�HRD͈RD��RD�RD�HRDΈRD��RD�RD�HRDψRD��RD�RD�HRDЈRD��RD�RD�HRDшRD��RD�RD�HRD҈RD��RD�RD�HRDӈRD��RD�RD�HRDԈRD��RD�RD�HRDՈRD��RD�RD�HRDֈRD��RD�RD�HRD׋�D��RD�D�HRD؈RD��RD�RD�HRDوRD��RD�RD�HRDڈRD��RD�RD�HRDۈRD��RD�RD�HRD܈RD��RD�RD�HRD݈RD��RD�RD�HRDވRD��RD�RD�HRD߈RD��RD�RD�HRD��RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD��RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD��D�;�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A��
A��
A���A�~�A�hsA��yA��A�\)Aщ7A�
=AͰ!A�r�A�=qA�t�A��A�A�A��A��uA�K�A��hA�A�S�A��A���A�{A�^5A��A��7A�ƨA���A��A�ZA�M�A�9XA���A��A��RA�z�A�1A���A��A��wA�A�XA�O�A�n�A��DA�Ay�^Ar{Al��Akp�AjI�Afn�AcO�Aa�hA_�mA]x�A[XAZ �AV�!AR�jAM��AE�hA@1'A=��A<Q�A:�!A8�DA9�A;p�A:~�A9dZA8�DA7K�A7��A6�9A5�7A4�A2A1VA.�jA-�;A-oA,bA*�9A*r�A*I�A)�A)dZA(��A(jA(ZA'\)A&��A&�jA&��A&ZA&(�A&z�A&5?A%"�A$�A$��A$ĜA$�9A$jA$bNA$^5A$-A#+A"�\A!t�A $�A|�Ax�A"�AM�A(�A;dA��A1'A�;A��A&�A�yAQ�A��A��Az�AE�A�A��A�PA%AoA|�A`BA��A^5A9XA�wAXA��AA�A�TA��A�DA�AffA(�A��A�
A�Al�A7LA&�A��AȴA��A/A��A{A1'A�mA��A�#Ap�A33AoA�A��A{A�A7LA�9AQ�AA`BAVA��AffA�mAO�A�A
�yA
�jA
��A
z�A
r�A
ZA
E�A
�A	�A	�;A	ƨA	�PA	;dA	%A��Az�A9XA1A�TA��AS�A/A��A�+A  AƨA�hA�PAO�A�`An�A1'A��A�#A��AXA?}A"�A��A��A�AA�A�;A��A&�A ȴA ��A V@��P@��@�^5@�p�@��@�bN@�|�@���@�O�@�I�@��F@���@��P@�+@��@��@�p�@���@�Q�@�b@�@���@�E�@��T@�p�@�&�@���@���@�@�Q�@��
@�dZ@���@�hs@�9@�A�@��;@띲@띲@�@��@�@� �@�t�@�C�@���@�p�@�%@���@�9@�@��;@�33@�@���@�~�@�J@�X@��@��`@��u@�b@�|�@ް!@�ff@�@�`B@�%@�Ĝ@�bN@�+@��H@��H@�$�@��@�@�7L@أ�@��m@�5?@պ^@Չ7@�x�@�X@ԣ�@�|�@�"�@�o@ҟ�@��T@�/@У�@Ѓ@�A�@ϕ�@�
=@Ο�@��@��#@�hs@�O�@�O�@���@̋D@�bN@� �@�  @��@�dZ@��@��@���@ə�@���@�r�@�dZ@��H@�^5@���@��`@�9X@�  @���@��@��@�X@���@�j@���@�;d@���@�J@�G�@�?}@��@��@�z�@�A�@�ƨ@�
=@���@�ff@��T@�&�@��P@���@�=q@�@���@�&�@�z�@� �@���@��@��@�v�@���@���@��^@�G�@�V@��@�r�@� �@���@�o@��+@�-@���@��@���@� �@�S�@�33@���@�{@��7@���@�r�@�I�@�A�@�1'@�1@��
@��F@��@���@�t�@�o@���@�V@��T@�x�@�G�@�/@�%@���@� �@���@�dZ@�S�@�@��R@�M�@���@���@��^@��-@��h@�hs@�?}@�7L@�7L@��@��`@���@���@�;d@��@��+@�^5@�5?@���@���@��h@�/@���@�A�@��w@�|�@�"�@��!@�M�@�@��h@�7L@��@��@��@�1'@���@�K�@�"�@��@���@�ff@�M�@�{@��@��h@�X@�7L@�V@���@�r�@�ƨ@�33@��@���@��@�@�G�@��@���@��u@�I�@��@��@��
@��@�\)@���@���@��+@���@�`B@��@��9@��@�(�@���@��@�dZ@�K�@�o@��\@��@��@�p�@�V@�Q�@�1'@�b@��m@��F@�|�@�l�@�K�@���@��!@�M�@�{@�@�x�@���@���@�j@�1'@��m@��@�dZ@�;d@��H@�^5@�{@���@�hs@��@��@�bN@�1'@�b@��@K�@~ȴ@~@}@}�-@}?}@|�j@|Z@{��@{33@z�@z��@z�\@zM�@yhs@x��@x �@w��@w�@wl�@w+@w
=@vv�@v{@u��@t��@t�j@tz�@s33@r�\@rM�@r-@q�@q��@qhs@q&�@p��@p�@o�;@o�w@o\)@o+@o�@n�R@m��@mp�@m�@l�j@k�m@kC�@j�@j�\@j=q@j�@i��@i%@h�u@h1'@g�@g�@g�@f��@fV@e��@ep�@dz�@c�
@cC�@b��@b�\@bM�@b-@a��@a��@a�@a�#@a��@ax�@a%@`�9@`Q�@_�@_K�@^��@]�T@]��@]�@]O�@\�@\�@\Z@\(�@[��@[��@[33@Z��@Z�!@Z-@Y�^@Y7L@Xr�@W�;@W��@W�P@WK�@W�@Vȴ@V��@V��@V�R@V�R@V�+@VE�@V5?@U�@U�T@Up�@T��@T��@T�@S�
@S��@S�@SC�@S"�@R�H@R��@R-@Q��@Q��@Qx�@QG�@Q�@P��@Pr�@O�;@O�@O|�@Ol�@O\)@OK�@O;d@O�@Nȴ@N5?@M��@M/@L�j@L9X@K��@Ko@J�!@J�@Ix�@I&�@I%@H��@H��@H��@Hr�@H �@G�;@GK�@F��@FV@F{@E�T@E@E�@E/@D��@D(�@C��@C�
@Cƨ@Cƨ@Cƨ@C�F@C��@C�@C@B��@BM�@A��@Ax�@@r�@@b@?��@?�P@?�@>$�@>{@>@=�@=�T@=��@=?}@<��@<�@<I�@<(�@;��@;33@:�@:�\@:�@9��@9�@9��@9�7@9hs@9X@9G�@9&�@8Ĝ@8bN@81'@7�@7�@7�P@7l�@7�@6��@6�@6v�@6{@5�h@5O�@5/@4��@4�D@4j@41@333@2��@2~�@2n�@2~�@2�\@2�!@2��@2��@2��@2�\@2n�@2M�@1��@1�^@1�7@0Ĝ@0Q�@0 �@0b@/�@/�P@/;d@/
=@.ȴ@.��@.V@-�T@-�-@-O�@-V@,�@,9X@,�@+�
@+��@+S�@*�@*��@*�!@*n�@*-@)��@)�@)��@)��@)hs@)�@(��@(�u@(�@(r�@( �@'�;@'�w@'�@'\)@&�y@&�@&�@&�R@&��@&5?@%�-@%p�@%?}@%�@%�@%V@%�@$�/@$�@$�D@$j@$�@#�F@#S�@#S�@#C�@#"�@#o@"��@"��@"~�@"n�@"M�@!�@!��@!��@!X@!&�@!�@!&�@ ��@ �u@ �@ Q�@ b@�w@�P@\)@+@�R@@��@�h@O�@?}@�@�/@��@z�@(�@��@�F@t�@�H@~�@M�@=q@�@J@�@��@�7@X@��@�@bN@Q�@A�@ �@b@  @�@�w@|�@;d@
=@��@�y@ȴ@��@�+@{@�T@p�@`B@?}@/@V@��@�/@��@z�@j@(�@�m@�F@�@C�@o@��@��@��@�!@�\@M�@J@��@�7@�7@X@&�@�@%@�@%@��@Ĝ@�u@r�@1'@ �@�;@l�@;d@�@��@�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A��
A��
A���A�~�A�hsA��yA��A�\)Aщ7A�
=AͰ!A�r�A�=qA�t�A��A�A�A��A��uA�K�A��hA�A�S�A��A���A�{A�^5A��A��7A�ƨA���A��A�ZA�M�A�9XA���A��A��RA�z�A�1A���A��A��wA�A�XA�O�A�n�A��DA�Ay�^Ar{Al��Akp�AjI�Afn�AcO�Aa�hA_�mA]x�A[XAZ �AV�!AR�jAM��AE�hA@1'A=��A<Q�A:�!A8�DA9�A;p�A:~�A9dZA8�DA7K�A7��A6�9A5�7A4�A2A1VA.�jA-�;A-oA,bA*�9A*r�A*I�A)�A)dZA(��A(jA(ZA'\)A&��A&�jA&��A&ZA&(�A&z�A&5?A%"�A$�A$��A$ĜA$�9A$jA$bNA$^5A$-A#+A"�\A!t�A $�A|�Ax�A"�AM�A(�A;dA��A1'A�;A��A&�A�yAQ�A��A��Az�AE�A�A��A�PA%AoA|�A`BA��A^5A9XA�wAXA��AA�A�TA��A�DA�AffA(�A��A�
A�Al�A7LA&�A��AȴA��A/A��A{A1'A�mA��A�#Ap�A33AoA�A��A{A�A7LA�9AQ�AA`BAVA��AffA�mAO�A�A
�yA
�jA
��A
z�A
r�A
ZA
E�A
�A	�A	�;A	ƨA	�PA	;dA	%A��Az�A9XA1A�TA��AS�A/A��A�+A  AƨA�hA�PAO�A�`An�A1'A��A�#A��AXA?}A"�A��A��A�AA�A�;A��A&�A ȴA ��A V@��P@��@�^5@�p�@��@�bN@�|�@���@�O�@�I�@��F@���@��P@�+@��@��@�p�@���@�Q�@�b@�@���@�E�@��T@�p�@�&�@���@���@�@�Q�@��
@�dZ@���@�hs@�9@�A�@��;@띲@띲@�@��@�@� �@�t�@�C�@���@�p�@�%@���@�9@�@��;@�33@�@���@�~�@�J@�X@��@��`@��u@�b@�|�@ް!@�ff@�@�`B@�%@�Ĝ@�bN@�+@��H@��H@�$�@��@�@�7L@أ�@��m@�5?@պ^@Չ7@�x�@�X@ԣ�@�|�@�"�@�o@ҟ�@��T@�/@У�@Ѓ@�A�@ϕ�@�
=@Ο�@��@��#@�hs@�O�@�O�@���@̋D@�bN@� �@�  @��@�dZ@��@��@���@ə�@���@�r�@�dZ@��H@�^5@���@��`@�9X@�  @���@��@��@�X@���@�j@���@�;d@���@�J@�G�@�?}@��@��@�z�@�A�@�ƨ@�
=@���@�ff@��T@�&�@��P@���@�=q@�@���@�&�@�z�@� �@���@��@��@�v�@���@���@��^@�G�@�V@��@�r�@� �@���@�o@��+@�-@���@��@���@� �@�S�@�33@���@�{@��7@���@�r�@�I�@�A�@�1'@�1@��
@��F@��@���@�t�@�o@���@�V@��T@�x�@�G�@�/@�%@���@� �@���@�dZ@�S�@�@��R@�M�@���@���@��^@��-@��h@�hs@�?}@�7L@�7L@��@��`@���@���@�;d@��@��+@�^5@�5?@���@���@��h@�/@���@�A�@��w@�|�@�"�@��!@�M�@�@��h@�7L@��@��@��@�1'@���@�K�@�"�@��@���@�ff@�M�@�{@��@��h@�X@�7L@�V@���@�r�@�ƨ@�33@��@���@��@�@�G�@��@���@��u@�I�@��@��@��
@��@�\)@���@���@��+@���@�`B@��@��9@��@�(�@���@��@�dZ@�K�@�o@��\@��@��@�p�@�V@�Q�@�1'@�b@��m@��F@�|�@�l�@�K�@���@��!@�M�@�{@�@�x�@���@���@�j@�1'@��m@��@�dZ@�;d@��H@�^5@�{@���@�hs@��@��@�bN@�1'@�b@��@K�@~ȴ@~@}@}�-@}?}@|�j@|Z@{��@{33@z�@z��@z�\@zM�@yhs@x��@x �@w��@w�@wl�@w+@w
=@vv�@v{@u��@t��@t�j@tz�@s33@r�\@rM�@r-@q�@q��@qhs@q&�@p��@p�@o�;@o�w@o\)@o+@o�@n�R@m��@mp�@m�@l�j@k�m@kC�@j�@j�\@j=q@j�@i��@i%@h�u@h1'@g�@g�@g�@f��@fV@e��@ep�@dz�@c�
@cC�@b��@b�\@bM�@b-@a��@a��@a�@a�#@a��@ax�@a%@`�9@`Q�@_�@_K�@^��@]�T@]��@]�@]O�@\�@\�@\Z@\(�@[��@[��@[33@Z��@Z�!@Z-@Y�^@Y7L@Xr�@W�;@W��@W�P@WK�@W�@Vȴ@V��@V��@V�R@V�R@V�+@VE�@V5?@U�@U�T@Up�@T��@T��@T�@S�
@S��@S�@SC�@S"�@R�H@R��@R-@Q��@Q��@Qx�@QG�@Q�@P��@Pr�@O�;@O�@O|�@Ol�@O\)@OK�@O;d@O�@Nȴ@N5?@M��@M/@L�j@L9X@K��@Ko@J�!@J�@Ix�@I&�@I%@H��@H��@H��@Hr�@H �@G�;@GK�@F��@FV@F{@E�T@E@E�@E/@D��@D(�@C��@C�
@Cƨ@Cƨ@Cƨ@C�F@C��@C�@C@B��@BM�@A��@Ax�@@r�@@b@?��@?�P@?�@>$�@>{@>@=�@=�T@=��@=?}@<��@<�@<I�@<(�@;��@;33@:�@:�\@:�@9��@9�@9��@9�7@9hs@9X@9G�@9&�@8Ĝ@8bN@81'@7�@7�@7�P@7l�@7�@6��@6�@6v�@6{@5�h@5O�@5/@4��@4�D@4j@41@333@2��@2~�@2n�@2~�@2�\@2�!@2��@2��@2��@2�\@2n�@2M�@1��@1�^@1�7@0Ĝ@0Q�@0 �@0b@/�@/�P@/;d@/
=@.ȴ@.��@.V@-�T@-�-@-O�@-V@,�@,9X@,�@+�
@+��@+S�@*�@*��@*�!@*n�@*-@)��@)�@)��@)��@)hs@)�@(��@(�u@(�@(r�@( �@'�;@'�w@'�@'\)@&�y@&�@&�@&�R@&��@&5?@%�-@%p�@%?}@%�@%�@%V@%�@$�/@$�@$�D@$j@$�@#�F@#S�@#S�@#C�@#"�@#o@"��@"��@"~�@"n�@"M�@!�@!��@!��@!X@!&�@!�@!&�@ ��@ �u@ �@ Q�@ b@�w@�P@\)@+@�R@@��@�h@O�@?}@�@�/@��@z�@(�@��@�F@t�@�H@~�@M�@=q@�@J@�@��@�7@X@��@�@bN@Q�@A�@ �@b@  @�@�w@|�@;d@
=@��@�y@ȴ@��@�+@{@�T@p�@`B@?}@/@V@��@�/@��@z�@j@(�@�m@�F@�@C�@o@��@��@��@�!@�\@M�@J@��@�7@�7@X@&�@�@%@�@%@��@Ĝ@�u@r�@1'@ �@�;@l�@;d@�@��@�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)�B
`BB
y�B
��B"�BQ�B|�B�DB��B�7BXB\)BBhBS�B{�B�PB�\B�=Bu�Bl�B[#BS�BO�BS�Bz�Be`B'�B�B��B��B��B��B��B�%B^5B#�BB
�B
�BB
��B
�LB
�1B
�B	ȴB	�\B	_;B	T�B	P�B	K�B	C�B	=qB	9XB	49B	0!B	/B	=qB	<jB	+B	�B�B�B	uB	�B	�B	/B	L�B	�B	��B	��B	�B	��B
�B
'�B
(�B
$�B
!�B
�B
uB
 �B
-B
<jB
;dB
=qB
?}B
@�B
C�B
@�B
=qB
D�B
H�B
I�B
L�B
P�B
W
B
[#B
e`B
jB
gmB
jB
k�B
p�B
x�B
~�B
� B
~�B
|�B
w�B
r�B
k�B
aHB
\)B
]/B
[#B
W
B
^5B
\)B
XB
W
B
W
B
XB
T�B
T�B
O�B
J�B
F�B
G�B
G�B
E�B
E�B
F�B
H�B
M�B
XB
ZB
W
B
T�B
T�B
R�B
R�B
R�B
O�B
K�B
L�B
M�B
M�B
M�B
N�B
P�B
P�B
O�B
O�B
P�B
S�B
XB
YB
bNB
iyB
r�B
{�B
~�B
}�B
~�B
}�B
|�B
z�B
z�B
y�B
x�B
v�B
t�B
s�B
q�B
p�B
n�B
k�B
hsB
e`B
bNB
_;B
\)B
ZB
XB
W
B
VB
T�B
S�B
S�B
R�B
S�B
T�B
T�B
T�B
R�B
Q�B
P�B
O�B
N�B
N�B
N�B
M�B
L�B
L�B
L�B
K�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
K�B
J�B
I�B
I�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
D�B
C�B
B�B
@�B
>wB
>wB
<jB
;dB
:^B
9XB
8RB
7LB
6FB
6FB
5?B
49B
33B
33B
33B
2-B
1'B
1'B
0!B
0!B
0!B
1'B
1'B
1'B
0!B
/B
/B
/B
/B
/B
/B
.B
.B
-B
-B
+B
(�B
'�B
'�B
'�B
'�B
&�B
&�B
$�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
uB
uB
uB
uB
uB
{B
uB
uB
uB
{B
{B
{B
uB
{B
{B
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
oB
bB
bB
bB
bB
\B
VB
VB
\B
hB
hB
hB
oB
hB
hB
hB
oB
hB
hB
hB
bB
\B
VB
PB
PB
JB
JB
JB
JB
DB
JB
JB
JB
JB
JB
DB
PB
VB
\B
\B
VB
\B
\B
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
hB
oB
oB
oB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
bB

=B
B
DB
	7B
B
  B	��B
B
%B
%B
B
+B
	7B
+B
DB
bB
JB

=B
%B
%B
\B
uB
oB
hB
{B
�B
oB
uB
bB
oB
{B
hB
PB
	7B
B
1B
bB
hB
VB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
bB
\B
oB
uB
1B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
oB
�B
{B
%�B
$�B
"�B
"�B
"�B
$�B
!�B
�B
�B
�B
 �B
�B
�B
�B
!�B
#�B
#�B
"�B
#�B
"�B
"�B
�B
�B
"�B
"�B
�B
 �B
'�B
(�B
,B
-B
+B
(�B
)�B
(�B
1'B
0!B
-B
+B
-B
+B
/B
2-B
2-B
0!B
.B
'�B
-B
.B
49B
5?B
2-B
2-B
0!B
+B
-B
-B
,B
/B
,B
"�B
0!B
7LB
9XB
6FB
7LB
7LB
6FB
5?B
2-B
33B
9XB
7LB
8RB
7LB
1'B
-B
5?B
5?B
33B
/B
49B
8RB
9XB
9XB
:^B
5?B
49B
7LB
9XB
:^B
9XB
5?B
7LB
8RB
5?B
5?B
1'B
7LB
;dB
>wB
B�B
B�B
D�B
E�B
F�B
E�B
C�B
A�B
=qB
<jB
=qB
<jB
;dB
8RB
:^B
=qB
D�B
E�B
D�B
A�B
C�B
C�B
D�B
D�B
A�B
A�B
B�B
B�B
?}B
?}B
?}B
>wB
C�B
H�B
J�B
H�B
I�B
I�B
L�B
M�B
L�B
J�B
G�B
G�B
H�B
E�B
E�B
@�B
A�B
C�B
D�B
H�B
J�B
I�B
H�B
I�B
G�B
F�B
E�B
G�B
K�B
J�B
I�B
I�B
H�B
E�B
F�B
M�B
N�B
P�B
O�B
N�B
L�B
H�B
E�B
B�B
D�B
B�B
D�B
D�B
D�B
F�B
H�B
G�B
I�B
O�B
Q�B
Q�B
Q�B
R�B
O�B
L�B
L�B
H�B
K�B
P�B
Q�B
Q�B
Q�B
P�B
N�B
M�B
Q�B
VB
XB
YB
ZB
YB
W
B
T�B
R�B
N�B
P�B
N�B
N�B
K�B
I�B
T�B
XB
R�B
Q�B
O�B
[#B
\)B
[#B
YB
W
B
VB
XB
W
B
VB
XB
T�B
VB
YB
XB
ZB
_;B
^5B
]/B
\)B
^5B
^5B
]/B
[#B
XB
ZB
\)B
\)B
]/B
^5B
]/B
[#B
]/B
[#B
XB
W
B
XB
\)B
]/B
ZB
[#B
\)B
W
B
T�B
\)B
dZB
gmB
hsB
iyB
iyB
gmB
e`B
dZB
cTB
cTB
aHB
^5B
_;B
^5B
YB
`BB
dZB
e`B
bNB
`BB
aHB
cTB
cTB
bNB
bNB
aHB
dZB
aHB
bNB
bNB
cTB
ffB
e`B
dZB
e`B
dZB
gmB
hsB
gmB
gmB
iyB
iyB
iyB
gmB
gmB
gmB
hsB
iyB
k�B
jB
hsB
iyB
jB
iyB
gmB
gmB
l�B
k�B
iyB
gmB
e`B
dZB
hsB
jB
k�B
m�B
l�B
jB
gmB
hsB
hsB
gmB
e`B
dZB
hsB
n�B
l�B
k�B
k�B
iyB
k�B
l�B
k�B
iyB
hsB
l�B
k�B
jB
l�B
n�B
m�B
jB
k�B
l�B
jB
jB
jB
k�B
jB
hsB
e`B
e`B
l�B
l�B
m�B
p�B
p�B
l�B
l�B
m�B
k�B
l�B
k�B
jB
iyB
m�B
q�B
r�B
q�B
q�B
p�B
o�B
o�B
n�B
l�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
q�B
q�B
r�B
t�B
u�B
u�B
s�B
s�B
r�B
p�B
s�B
r�B
w�B
w�B
w�B
w�B
w�B
v�B
t�B
v�B
v�B
t�B
u�B
v�B
v�B
v�B
v�B
w�B
z�B
z�B
y�B
w�B
u�B
w�B
v�B
{�B
{�B
z�B
{�B
|�B
}�B
~�B
~�B
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
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B
tB
tB
tB
tB
tB
tB
tB
tB
}B
vB
�B
�B
�B
�B
�B
�B
)�B
`4B
y�B
��B"�BQ�B|�B�1B��B�"BW�B\B�yBRBS�B{�B�:B�GB�'Bu�BluB[BS�BO�BS�Bz�BeIB'�B��B��B��B�}B��B��B�B^B#�B�B
�B
�-B
��B
�6B
�B
�B	ȥB	�PB	_.B	T�B	P�B	K�B	C�B	=dB	9LB	4,B	0B	/B	=eB	<\B	*�B	�B�B�B	fB	�B	�B	/B	L�B	�
B	��B	��B	�	B	��B
�B
'�B
(�B
$�B
!�B
{B
]B
 �B
,�B
<QB
;LB
=YB
?cB
@lB
C|B
@iB
=\B
D�B
H�B
I�B
L�B
P�B
V�B
[	B
eEB
jeB
gUB
jdB
kkB
p�B
x�B
~�B
�B
~�B
|�B
w�B
r�B
kjB
a-B
\B
]B
[
B
V�B
^B
\B
W�B
V�B
V�B
W�B
T�B
T�B
O�B
J�B
F�B
G�B
G�B
E�B
E�B
F�B
H�B
M�B
W�B
ZB
V�B
T�B
T�B
R�B
R�B
R�B
O�B
K�B
L�B
M�B
M�B
M�B
N�B
P�B
P�B
O�B
O�B
P�B
S�B
W�B
X�B
b4B
i]B
r�B
{�B
~�B
}�B
~�B
}�B
|�B
z�B
z�B
y�B
x�B
v�B
t�B
s�B
q�B
p�B
n|B
khB
hVB
eCB
b4B
_B
\B
ZB
W�B
V�B
U�B
T�B
S�B
S�B
R�B
S�B
T�B
T�B
T�B
R�B
Q�B
P�B
O�B
N�B
N�B
N�B
M�B
L�B
L�B
L�B
K�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
K�B
J�B
I�B
I�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
D�B
C~B
BvB
@iB
>\B
>]B
<NB
;KB
:BB
9>B
88B
71B
6,B
6-B
5%B
4 B
3B
3B
3B
2B
1B
1B
0B
0B
0B
1B
1B
1B
0B
.�B
/B
/B
/ B
/ B
/B
-�B
-�B
,�B
,�B
*�B
(�B
'�B
'�B
'�B
'�B
&�B
&�B
$�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
yB
wB
zB
zB
tB
qB
{B
yB
zB
qB
yB
�B
wB
sB
tB
tB
tB
tB
yB
�B
B
|B
mB
YB
YB
\B
\B
\B
[B
ZB
aB
[B
[B
[B
_B
_B
aB
\B
^B
bB
ZB
ZB
[B
[B
[B
YB
ZB
XB
ZB
[B
\B
\B
TB
IB
HB
HB
HB
BB
<B
;B
BB
KB
NB
LB
TB
KB
MB
NB
TB
MB
MB
OB
HB
BB
9B
6B
6B
1B
0B
/B
/B
(B
0B
.B
0B
0B
0B
)B
6B
;B
BB
@B
:B
AB
AB
HB
IB
GB
JB
NB
UB
SB
RB
QB
TB
RB
QB
TB
RB
VB
TB
UB
RB
VB
KB
RB
SB
SB
RB
ZB
]B
`B
_B
eB
hB
cB
dB
kB
kB
mB
lB
nB
kB
gB
dB
kB
rB
sB
sB
qB
yB
xB
�B
B
B
�B
yB
xB
}B
xB
~B
~B
B
}B
�B
~B
�B
zB
wB
vB
zB
~B
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.26 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544322016080715443220160807154432  AO  ARCAADJP                                                                    20150226221444    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221444  QCP$                G�O�G�O�G�O�8FB5E           AO  ARGQQCPL                                                                    20150226221444  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154432  IP                  G�O�G�O�G�O�                