CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:38Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142951  20190522121827  1727_5046_168                   2C  D   APEX                            2143                            040306                          846 @���� 1   @������@6�\(��c����+1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��DsffDy��D�&fD�\�D���D�� D�)�D�\�D���D��3D�#3D�l�D��fD��3D�  D�S3Dڣ3D�ɚD�,�D�` D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B���B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2�3D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��DsfDss3Dy��D�,�D�c3D�� D��fD�0 D�c3D�� D���D�)�D�s3D���D�ٚD�&fD�Y�Dک�D�� D�33D�ffD�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�|�A�z�A�n�A�M�A�dZA�K�A�C�A�(�A�oA��A�ȴAѾwAѶFAѲ-AѮAѥ�Aћ�AёhAщ7AсA�p�A�jA�XA�-A���AЩ�A�$�A��AȾwA���A��yA�ffA���A�VA��yA�E�A���A���A�ƨA�XA��TA�dZA�G�A���A���A�^5A�(�A��A�{A��;A���A���A�XA��FA�|�A�A�A��A��yA��;A��^A���A�jA�5?A���A��!A�Q�A��A�ĜA��hA�A�A�A��;A��!A�r�A�9XA�-A�1A��DA�bNA�E�A��A��`A���A�n�A���A�ȴA���A��9A��\A��HA�z�A���A�ffA�33A���A�JA��DA���A��A���A���A��wA���A�1'A��A��A��uA�dZA�&�A��9A��A���A���A�7LA�ƨA�7LA�ȴA�-A��A�VA���A�E�A�ƨA��^A��uA���A�ȴA�I�A�n�A�  A��DA���A��;A�33A���A��PA�5?A�r�A}�A{C�Az1'Axn�Av��AvbAuXAtffAs|�Ar-Ap�DAoO�An�\AnE�Am�hAl�Ak&�Af�`Ac�A`��AZ��AU��AT �AQ��AO�#ALĜAF�HAF{AEƨAC��AAƨA@ffA>^5A=��A=+A<A�A:��A9
=A7��A7�hA7%A4�uA3XA2��A2I�A2=qA2(�A2�A1�PA0bA.Q�A-�PA,^5A)��A(bNA'�A&�!A%�A$�yA#��A"VA!��A!��A!��A!hsA ��A�TA\)AVAO�A��A1A�PA�A9XA�wA�A33A��A��A�+At�A=qAO�A�TA��AdZA	�mA	C�Az�A1A�A��A�hA?}AVA�9AdZAO�A I�@��y@�j@�@���@��;@���@�&�@���@�5?@�P@�@��@�n�@�7L@�Ĝ@��y@��@�?}@���@��@�z�@�A�@���@�C�@�\@�7L@�v�@�G�@���@�r�@ۮ@���@�hs@؃@�A�@��@�ȴ@�x�@�%@ԛ�@���@ҸR@�G�@�Z@���@ϝ�@�o@��H@·+@�X@��@�1@���@�V@���@�G�@�/@�V@���@�A�@ǍP@�t�@�K�@ƸR@�V@öF@�+@�@\@�z�@��9@�p�@��@���@�@��!@�V@�{@�/@��`@���@��j@���@��D@�bN@�t�@��R@�v�@�$�@�J@��T@�`B@�hs@��@��@��@�bN@�I�@�A�@��@�  @��w@�S�@��H@���@�E�@�@��@���@�1'@��P@�o@��@���@�5?@���@��@��`@�j@�bN@�  @�K�@��@�@��y@�ȴ@���@���@���@�7L@��`@�z�@�r�@�j@�bN@�Z@�9X@��@��;@�ƨ@��w@��F@���@�|�@�o@���@���@���@�~�@�v�@�v�@�~�@���@�~�@�V@�=q@�-@��@���@�@�X@�1'@�ƨ@��@�dZ@�"�@��@�"�@��H@�^5@���@���@�@��-@��@�hs@�O�@�?}@�/@�V@��@��`@��j@��@�9X@��@��m@���@���@�|�@��@��@�dZ@�33@�
=@��@��@��R@�n�@�J@���@��h@�x�@��@�x�@�`B@�/@��`@�z�@� �@��
@���@���@��@�
=@��y@�ff@���@��j@�z�@�z�@��@��D@��@�Z@�A�@� �@���@�
=@��R@��+@�n�@�=q@�J@��@���@��^@�7L@���@��/@�Ĝ@�Q�@�dZ@��!@�ff@�E�@�@��@��\@���@v��@j=q@c�F@Z��@T9X@LZ@F��@@b@9%@4I�@/+@(r�@"��@E�@��@�@%@C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�|�A�|�A�z�A�n�A�M�A�dZA�K�A�C�A�(�A�oA��A�ȴAѾwAѶFAѲ-AѮAѥ�Aћ�AёhAщ7AсA�p�A�jA�XA�-A���AЩ�A�$�A��AȾwA���A��yA�ffA���A�VA��yA�E�A���A���A�ƨA�XA��TA�dZA�G�A���A���A�^5A�(�A��A�{A��;A���A���A�XA��FA�|�A�A�A��A��yA��;A��^A���A�jA�5?A���A��!A�Q�A��A�ĜA��hA�A�A�A��;A��!A�r�A�9XA�-A�1A��DA�bNA�E�A��A��`A���A�n�A���A�ȴA���A��9A��\A��HA�z�A���A�ffA�33A���A�JA��DA���A��A���A���A��wA���A�1'A��A��A��uA�dZA�&�A��9A��A���A���A�7LA�ƨA�7LA�ȴA�-A��A�VA���A�E�A�ƨA��^A��uA���A�ȴA�I�A�n�A�  A��DA���A��;A�33A���A��PA�5?A�r�A}�A{C�Az1'Axn�Av��AvbAuXAtffAs|�Ar-Ap�DAoO�An�\AnE�Am�hAl�Ak&�Af�`Ac�A`��AZ��AU��AT �AQ��AO�#ALĜAF�HAF{AEƨAC��AAƨA@ffA>^5A=��A=+A<A�A:��A9
=A7��A7�hA7%A4�uA3XA2��A2I�A2=qA2(�A2�A1�PA0bA.Q�A-�PA,^5A)��A(bNA'�A&�!A%�A$�yA#��A"VA!��A!��A!��A!hsA ��A�TA\)AVAO�A��A1A�PA�A9XA�wA�A33A��A��A�+At�A=qAO�A�TA��AdZA	�mA	C�Az�A1A�A��A�hA?}AVA�9AdZAO�A I�@��y@�j@�@���@��;@���@�&�@���@�5?@�P@�@��@�n�@�7L@�Ĝ@��y@��@�?}@���@��@�z�@�A�@���@�C�@�\@�7L@�v�@�G�@���@�r�@ۮ@���@�hs@؃@�A�@��@�ȴ@�x�@�%@ԛ�@���@ҸR@�G�@�Z@���@ϝ�@�o@��H@·+@�X@��@�1@���@�V@���@�G�@�/@�V@���@�A�@ǍP@�t�@�K�@ƸR@�V@öF@�+@�@\@�z�@��9@�p�@��@���@�@��!@�V@�{@�/@��`@���@��j@���@��D@�bN@�t�@��R@�v�@�$�@�J@��T@�`B@�hs@��@��@��@�bN@�I�@�A�@��@�  @��w@�S�@��H@���@�E�@�@��@���@�1'@��P@�o@��@���@�5?@���@��@��`@�j@�bN@�  @�K�@��@�@��y@�ȴ@���@���@���@�7L@��`@�z�@�r�@�j@�bN@�Z@�9X@��@��;@�ƨ@��w@��F@���@�|�@�o@���@���@���@�~�@�v�@�v�@�~�@���@�~�@�V@�=q@�-@��@���@�@�X@�1'@�ƨ@��@�dZ@�"�@��@�"�@��H@�^5@���@���@�@��-@��@�hs@�O�@�?}@�/@�V@��@��`@��j@��@�9X@��@��m@���@���@�|�@��@��@�dZ@�33@�
=@��@��@��R@�n�@�J@���@��h@�x�@��@�x�@�`B@�/@��`@�z�@� �@��
@���@���@��@�
=@��y@�ff@���@��j@�z�@�z�@��@��D@��@�Z@�A�@� �@���@�
=@��R@��+@�n�@�=q@�J@��@���@��^@�7L@���@��/@�Ĝ@�Q�@�dZ@��!@�ff@�E�@�@��@��\@���@v��@j=q@c�F@Z��@T9X@LZ@F��@@b@9%@4I�@/+@(r�@"��@E�@��@�@%@C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBp�Bp�Bp�Bo�Bp�Bo�Bo�Bo�Bp�Bo�Bo�Bo�Bn�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bk�BjBhsBe`BcTB_;BVBB�B6FB33B6FB>wBL�BP�BXBm�Bs�Bz�B�B�B�%B�VB�JB�7B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�-B��BÖBŢBȴB��B��B��B�B�B�B�B�B�5B�)B�B��B��B��BǮB��B�?B�B��B��B�VB� Bs�BbNBYBVBVBQ�BL�BF�B?}B<jB7LB1'B)�B!�BDB�B�)B��B��BĜB��B�{B�BgmBP�BB�B+B�BB
��B
ȴB
��B
�B
p�B
VB
5?B
&�B
�B
uB
+B
B	��B	��B	�B	�yB	�;B	�B	��B	��B	��B	ƨB	�^B	��B	�\B	|�B	_;B	A�B	7LB	'�B	�B	VB��B��B��B�yB�BB�
B��B��BɺBÖB�wB�LB�!B�B��B��B��B�uB�uB�{B��B�uB�oB�JB�1B�B~�B}�B}�B|�B{�By�Bw�Bt�Bv�Bx�By�Bx�Bx�By�B{�B{�B|�B|�B|�B|�B|�B{�B~�B~�B~�B}�B{�Bz�By�Bw�Bs�Bs�Bt�Bs�Bs�Bt�Bv�Bw�By�By�By�Bx�Bw�Bx�Bv�Bu�Bz�By�Bt�Bp�Bn�Bl�Bk�BjBhsBgmBgmBk�Bm�Bn�Bo�Bo�Bn�Bn�Bn�Bn�Bo�Bn�Bn�Bn�Bm�Bm�Bm�Bm�Br�Bv�Bv�Bw�By�Bz�B~�B�B� B~�B�B�7B�DB�JB�VB�hB��B��B��B��B��B��B��B�-B�?B�^B��BBƨBȴB��B��B��B��B�B�B�B�
B�)B�5B�5B�5B�)B�BɺBȴBȴB��B��B��B��B��B�
B�B�#B�)B�/B�5B�5B�ZB�yB�B�B�B�B�B��B	B	1B	
=B	JB	JB	PB	VB	\B	hB	�B	�B	�B	�B	!�B	%�B	(�B	+B	/B	1'B	1'B	2-B	49B	6FB	7LB	:^B	@�B	A�B	D�B	H�B	I�B	I�B	I�B	I�B	I�B	J�B	K�B	M�B	O�B	R�B	R�B	R�B	R�B	R�B	S�B	XB	YB	[#B	[#B	\)B	^5B	]/B	^5B	`BB	`BB	aHB	e`B	ffB	gmB	hsB	k�B	p�B	q�B	q�B	r�B	u�B	v�B	w�B	y�B	|�B	� B	� B	�B	�1B	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�RB	�^B	�dB	�jB	�jB	�jB	�qB	�}B	��B	��B	��B	��B	��B	��B	��B	ÖB	ŢB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�mB	��B
+B
{B
�B
"�B
)�B
0!B
7LB
>wB
F�B
K�B
Q�B
T�B
ZB
`BB
e`B
k�B
n�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bp�Bp�Bp�Bo�Bn�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bk�BjBiyBffBdZBaHBaHBQ�B=qB8RB9XBC�BN�BQ�BZBo�Bw�B~�B�B�B�1B�\B�PB�VB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B��B�B��B��B��B�B�B�B�!B�9B��BÖBƨBɺB��B��B�
B�B�B�B�#B�)B�BB�;B�B��B��B��B��BŢB�^B�!B��B��B�hB�Bx�BffBZBW
BW
BS�BO�BJ�BA�B>wB9XB49B,B)�B�B�B�5B�
B��B��B�B��B�PBn�BT�BI�B-B(�B1BB
�#B
��B
�PB
|�B
e`B
>wB
+B
$�B
�B

=B
%B
B	��B	��B	�B	�TB	�#B	��B	��B	��B	��B	ŢB	��B	��B	�7B	jB	D�B	<jB	,B	&�B	�B��B��B��B�B�ZB�/B��B��B��BȴBÖB�dB�!B�B�!B��B��B�{B�uB�{B��B�{B��B�bB�=B�+B�B�B~�B� B~�B{�Bz�Bx�Bw�By�By�By�Bz�B{�B}�B~�B� B~�B~�B~�B�B�B�B� B� B� B~�B}�B|�B|�B|�Bx�Bw�Bx�Bw�Bv�Bx�Bx�By�By�Bz�By�By�B|�By�Bz�B}�B{�Bw�Bs�Bp�Bm�Bm�Bm�Bl�Bn�Bk�Bk�Bm�Bp�Bq�Bp�Bq�Bp�Bo�Bo�Bo�Bn�Bo�Bo�Bn�Bo�Bp�Br�Bt�Bw�Bw�By�B{�B}�B�B�B�B�B�B�=B�JB�VB�bB�{B��B��B��B��B��B��B��B�3B�FB�dB��BÖBǮBȴB��B��B��B��B�B�B�
B�B�5B�;B�5B�;B�BB�)B��BɺB��B��B��B��B��B��B�B�B�#B�)B�/B�5B�BB�`B�yB�B�B�B�B�B��B	B	1B	
=B	JB	JB	PB	VB	\B	oB	�B	�B	�B	�B	"�B	&�B	)�B	,B	0!B	1'B	2-B	33B	5?B	6FB	7LB	;dB	@�B	A�B	E�B	H�B	I�B	I�B	I�B	I�B	I�B	K�B	L�B	M�B	P�B	R�B	R�B	R�B	R�B	R�B	S�B	XB	YB	[#B	[#B	\)B	^5B	^5B	_;B	`BB	`BB	aHB	e`B	ffB	gmB	hsB	k�B	p�B	q�B	q�B	r�B	u�B	v�B	x�B	{�B	}�B	� B	� B	�B	�1B	�JB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�RB	�RB	�^B	�dB	�jB	�jB	�jB	�qB	�}B	B	B	��B	��B	��B	B	��B	ÖB	ƨB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�mB	��B
+B
{B
�B
"�B
)�B
0!B
7LB
>wB
F�B
K�B
Q�B
T�B
ZB
`BB
e`B
k�B
n�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�t�<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447332012010314473320120103144733  AO  ARGQ                                                                        20111130142951  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142951  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144733  IP                  G�O�G�O�G�O�                