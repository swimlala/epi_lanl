CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:03Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  YP   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  hh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190503  20181005190503  5904951 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6430                            2B  A   APEX                            7465                            062512                          846 @מh>�x1   @מh���@4�I�^5�c��S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�=D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @r�\@�{@�{A
=A;
=A[
=A{
=A��A��A�Q�A��A�Q�A݅A�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO�>CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�>Cg�>Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw��DyuD�33D�ָ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A\AuAuAhAhAuA\AuA\A�A�ADA�|�A�E�A�"�A�A��;A��RA���A��7A�~�A�|�A�t�A�ffA�S�A�I�A�9XA�-A� �A�oA�
=A�A���A��A��^A�x�A�5?A��A�t�A�(�A�%A��jA�G�A�&�A���A��7A�%A��wA�|�A�n�A�S�A��A�A���A�K�A���A�r�A���A�G�A��A��jA�jA��;A�E�A�1A���A���A��A��A� �A�C�A���A���A���A�bA��HA�G�A��\A���A�A���A�ƨA���A��7A���A��7A��7A�ȴA�/A�jA��yA�JA�|�A��hA�&�A��#A�-A�%A��`A��A�JA�dZA�ȴA�ȴA���A��AzbAwC�Aq�-An�DAnbNAk�TAh �Ad��Ac��Ac�A_\)AZ��AW%AP�uAL�!AJn�AEXAB~�AAG�A@$�A>��A>A=/A<��A<n�A:E�A85?A6��A3�TA2v�A2{A0�A/+A.ĜA-��A,A*�A*1'A)�wA)x�A)K�A)oA(��A(bA&ȴA"I�AȴA�^A�#AdZAt�AbNAȴAZA5?A�TA�uA�FA�A�^A�AQ�A��AjA��Ap�A�yAM�A&�A
�A
1'A	
=A �A�A�PA&�A{Ap�A%A�RAI�A9XA-A �A��A�wA�AO�AVAAA��A�yA�RA�A ��A ��A ��A �jA ~�A (�@��R@�J@���@�X@��;@���@�o@�G�@�{@�+@�@���@�7@�7L@��@��@�E�@���@���@�O�@� �@�v�@��H@�P@��@��@�bN@�@��@�ȴ@�V@�J@�|�@߶F@���@�(�@܃@��@�X@ݑh@ڟ�@�33@ָR@��@�X@�r�@�G�@Ѻ^@��`@Ͼw@�E�@̴9@��;@�v�@��@��@��#@�{@Ɂ@�Ĝ@�r�@��@���@�33@�`B@�=q@���@�X@���@��D@�b@�l�@��y@�$�@�z�@� �@�1@���@���@��!@�33@���@�v�@���@�;d@��R@��+@���@��j@��m@���@�ff@��@��h@��9@��@�ƨ@���@�l�@�K�@��@���@�E�@�5?@��@��@��@��@�$�@�=q@�ff@���@���@���@���@���@��-@�9X@�n�@��@�J@�E�@�-@��T@���@��h@�?}@�$�@�J@�&�@�%@�hs@��@���@���@�hs@��@���@�Ĝ@��9@�j@�1'@��@��;@��
@�ƨ@��@���@��@�V@���@��h@��7@��@�  @�1@�1'@��@��m@��m@�ƨ@��P@�|�@�l�@�l�@�33@�M�@�hs@�&�@��j@��@��u@�z�@�A�@��@��m@��F@�t�@�v�@�E�@�5?@�@��#@�@���@���@�p�@�r�@�o@�~�@�^5@�E�@��#@�p�@���@��@��H@���@��\@�ff@�=q@���@���@�j@�(�@��
@�K�@�K�@�\)@�C�@�+@�
=@���@�^5@�{@�@���@���@�p�@��M@q��@`I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111   A\AuAuAhAhAuA\AuA\A�A�ADA�|�A�E�A�"�A�A��;A��RA���A��7A�~�A�|�A�t�A�ffA�S�A�I�A�9XA�-A� �A�oA�
=A�A���A��A��^A�x�A�5?A��A�t�A�(�A�%A��jA�G�A�&�A���A��7A�%A��wA�|�A�n�A�S�A��A�A���A�K�A���A�r�A���A�G�A��A��jA�jA��;A�E�A�1A���A���A��A��A� �A�C�A���A���A���A�bA��HA�G�A��\A���A�A���A�ƨA���A��7A���A��7A��7A�ȴA�/A�jA��yA�JA�|�A��hA�&�A��#A�-A�%A��`A��A�JA�dZA�ȴA�ȴA���A��AzbAwC�Aq�-An�DAnbNAk�TAh �Ad��Ac��Ac�A_\)AZ��AW%AP�uAL�!AJn�AEXAB~�AAG�A@$�A>��A>A=/A<��A<n�A:E�A85?A6��A3�TA2v�A2{A0�A/+A.ĜA-��A,A*�A*1'A)�wA)x�A)K�A)oA(��A(bA&ȴA"I�AȴA�^A�#AdZAt�AbNAȴAZA5?A�TA�uA�FA�A�^A�AQ�A��AjA��Ap�A�yAM�A&�A
�A
1'A	
=A �A�A�PA&�A{Ap�A%A�RAI�A9XA-A �A��A�wA�AO�AVAAA��A�yA�RA�A ��A ��A ��A �jA ~�A (�@��R@�J@���@�X@��;@���@�o@�G�@�{@�+@�@���@�7@�7L@��@��@�E�@���@���@�O�@� �@�v�@��H@�P@��@��@�bN@�@��@�ȴ@�V@�J@�|�@߶F@���@�(�@܃@��@�X@ݑh@ڟ�@�33@ָR@��@�X@�r�@�G�@Ѻ^@��`@Ͼw@�E�@̴9@��;@�v�@��@��@��#@�{@Ɂ@�Ĝ@�r�@��@���@�33@�`B@�=q@���@�X@���@��D@�b@�l�@��y@�$�@�z�@� �@�1@���@���@��!@�33@���@�v�@���@�;d@��R@��+@���@��j@��m@���@�ff@��@��h@��9@��@�ƨ@���@�l�@�K�@��@���@�E�@�5?@��@��@��@��@�$�@�=q@�ff@���@���@���@���@���@��-@�9X@�n�@��@�J@�E�@�-@��T@���@��h@�?}@�$�@�J@�&�@�%@�hs@��@���@���@�hs@��@���@�Ĝ@��9@�j@�1'@��@��;@��
@�ƨ@��@���@��@�V@���@��h@��7@��@�  @�1@�1'@��@��m@��m@�ƨ@��P@�|�@�l�@�l�@�33@�M�@�hs@�&�@��j@��@��u@�z�@�A�@��@��m@��F@�t�@�v�@�E�@�5?@�@��#@�@���@���@�p�@�r�@�o@�~�@�^5@�E�@��#@�p�@���@��@��H@���@��\@�ff@�=q@���@���@�j@�(�@��
@�K�@�K�@�\)@�C�@�+@�
=@���@�^5@�{@�@���@���@�p�@��M@q��@`I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B��B��B��B�B��BB�B!�B+B5?BA�BXBffBhsBiyBjBjBk�Bk�Bl�Bm�Bn�Bp�Bq�Bq�Br�Bt�Bx�B�B�+B�+B�B�B�B�B�B�B� B�B�%B�7B�JB�uB��B�?BÖBȴB��B�)B�TB�B��BBBPB�B�B�B{BbBB��B�)B��B�wB�3B��B��B�{B�VB�%B{�BhsBZB-B�B��B�B�RB|�BQ�BL�BH�BB�B=qB7LB&�BbBB
�mB
ƨB
��B
}�B
m�B
aHB
R�B
49B
 �B	��B	��B	�^B	��B	�7B	�1B	v�B	\)B	J�B	E�B	@�B	(�B	\B��B�;B��BŢB�wB�^B�LB�?B�?B�FB�jB�qB�qB�jB�dB�^B��BǮBȴB��B��B��B��B�B�)B�BB�NB�`B�`B�ZB�TB�;B��B�wB��B��B��B�=B�7B�B�B�B� B~�B� B�B�B�7B�7B�1B�%B�%B�7B�DB�JB�JB�\B�\B�VB�PB�bB�\B�VB�VB�hB�hB�hB�hB�oB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�FB�}BŢBƨBȴBɺB��B��B�#B�BB�TB�B��B��B��B	+B	uB	�B	�B	!�B	"�B	�B	�B	{B	�B	�B	�B	�B	"�B	'�B	,B	1'B	-B	$�B	&�B	'�B	�B	�B	�B	#�B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	'�B	'�B	&�B	&�B	&�B	&�B	%�B	&�B	+B	+B	-B	/B	/B	/B	5?B	7LB	9XB	8RB	5?B	6FB	:^B	<jB	<jB	<jB	=qB	<jB	<jB	=qB	>wB	@�B	B�B	C�B	E�B	F�B	H�B	K�B	N�B	N�B	O�B	P�B	P�B	P�B	Q�B	T�B	ZB	`BB	aHB	bNB	cTB	dZB	aHB	`BB	]/B	`BB	e`B	iyB	l�B	p�B	s�B	s�B	J�B	�oB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�RB	�XB	�XB	�dB	�jB	�wB	��B	�;B	�BB	�BB	�HB	�HB	�BB	�NB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	G�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
+B
%B
%B
%B
%B
DB
�B
�B
+�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222   B�B�B�B�B�B�B�B��B��B��B�B��BB�B!�B+B5?BA�BXBffBhsBiyBjBjBk�Bk�Bl�Bm�Bn�Bp�Bq�Bq�Br�Bt�Bx�B�B�+B�+B�B�B�B�B�B�B� B�B�%B�7B�JB�uB��B�?BÖBȴB��B�)B�TB�B��BBBPB�B�B�B{BbBB��B�)B��B�wB�3B��B��B�{B�VB�%B{�BhsBZB-B�B��B�B�RB|�BQ�BL�BH�BB�B=qB7LB&�BbBB
�mB
ƨB
��B
}�B
m�B
aHB
R�B
49B
 �B	��B	��B	�^B	��B	�7B	�1B	v�B	\)B	J�B	E�B	@�B	(�B	\B��B�;B��BŢB�wB�^B�LB�?B�?B�FB�jB�qB�qB�jB�dB�^B��BǮBȴB��B��B��B��B�B�)B�BB�NB�`B�`B�ZB�TB�;B��B�wB��B��B��B�=B�7B�B�B�B� B~�B� B�B�B�7B�7B�1B�%B�%B�7B�DB�JB�JB�\B�\B�VB�PB�bB�\B�VB�VB�hB�hB�hB�hB�oB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�FB�}BŢBƨBȴBɺB��B��B�#B�BB�TB�B��B��B��B	+B	uB	�B	�B	!�B	"�B	�B	�B	{B	�B	�B	�B	�B	"�B	'�B	,B	1'B	-B	$�B	&�B	'�B	�B	�B	�B	#�B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	'�B	'�B	&�B	&�B	&�B	&�B	%�B	&�B	+B	+B	-B	/B	/B	/B	5?B	7LB	9XB	8RB	5?B	6FB	:^B	<jB	<jB	<jB	=qB	<jB	<jB	=qB	>wB	@�B	B�B	C�B	E�B	F�B	H�B	K�B	N�B	N�B	O�B	P�B	P�B	P�B	Q�B	T�B	ZB	`BB	aHB	bNB	cTB	dZB	aHB	`BB	]/B	`BB	e`B	iyB	l�B	p�B	s�B	s�B	J�B	�oB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�RB	�XB	�XB	�dB	�jB	�wB	��B	�;B	�BB	�BB	�HB	�HB	�BB	�NB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	G�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
+B
%B
%B
%B
%B
DB
�B
�B
+�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190503                              AO  ARCAADJP                                                                    20181005190503    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190503  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190503  QCF$                G�O�G�O�G�O�C000            