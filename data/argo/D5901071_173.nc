CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:39Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143056  20190522121827  1727_5046_173                   2C  D   APEX                            2143                            040306                          846 @��A�j��1   @��C/hP@5~��"���c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`ffBhffBpffBw��B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dry�Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0ffB8��B@��BH��BP��BX��Ba33Bi33Bq33BxffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�33B�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CPL�CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB3DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[3D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do3Do��Dp�Dp��Dq�Dq��Dr�Dr�fDs�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AնFAոRAե�A�~�A�bNA�A�A��A���A��;A��yA��A���A�%A�  A��A��`A��#A��;A���A���A���A���AԴ9AԲ-AԮAԗ�AԁA�VAғuA�K�A�$�A�ffA�C�A��A��yA��uA��A�bA��9A��uA�&�A�A��A��\A���A��-A�7LA�VA��9A��A�oA���A��A�ĜA�
=A�E�A��+A�I�A�1'A�I�A��A���A�=qA�oA�A��HA�ZA��FA�%A�dZA�{A��A�bNA��yA�~�A�K�A��A��jA���A���A� �A�bA��A���A���A��A�VA�VA���A�A��A�x�A��TA�A��RA��RA���A���A��uA���A���A��FA���A�C�A���A�A��hA�"�A��A���A��A���A���A��A�5?A�bA��
A��FA}�A{x�Avn�ApjAm�PAl�Al$�Ajr�Ah�yAe�Aa�^A`n�A_%A^bA]�TA]�FA\�HA\A�AZQ�AX�AVffATffAS7LAR�DAQ��AQ�7AQ�APVANE�AM�PAJQ�AF�uADA�AA`BA?�A>r�A=O�A;��A:��A:jA:Q�A:�A9�TA97LA69XA3�wA1+A/�mA/A/�A.VA,ȴA*^5A)XA(^5A'��A'�hA'dZA&JA%&�A$�\A$$�A#ƨA#p�A#oA"Q�A!��A ��AAQ�A��A-A"�Az�A��A�A{A%A1A�jA��AA�-A7LAE�A�;A�wA�AA
�DA	��A�AC�A��AffA5?A�mAA�FA�A��A��A�hAt�A�A��A��AQ�A�yA�
A �/@�E�@�?}@�j@�1'@��@�@���@�Ĝ@��j@�I�@�v�@���@��@�S�@��^@�D@��@�^@�/@�ƨ@�\)@�+@�S�@�-@�  @�@݉7@��@�;d@��@ڧ�@�{@�z�@���@�"�@֟�@Ձ@�(�@�C�@҇+@�@љ�@��@�z�@�1'@���@�~�@�p�@�1'@�C�@��y@��@ɩ�@ɡ�@�x�@�7L@���@ȃ@�j@�b@ǥ�@ǅ@�;d@�
=@�v�@�@�V@ě�@�9X@�ƨ@�l�@���@�7L@��j@��u@�j@�(�@�ff@�5?@��T@�S�@�X@��m@���@���@�bN@��@�;d@�"�@��T@��@��@��w@��@�K�@��y@�=q@���@�~�@�~�@�5?@�@�7L@��@�%@�%@��@��u@�1'@���@�"�@�n�@�V@�-@��R@��@�1'@���@�t�@�C�@��@��@���@���@�E�@�%@�1@��w@��F@��@���@�@�V@��#@�x�@�p�@�X@�/@�V@���@��/@�Ĝ@��u@��@�1'@��w@���@�|�@�C�@�"�@�@�E�@��@�@�{@�@�?}@���@��@�j@�Z@�Q�@�(�@��@��;@�ƨ@���@��@�33@�ȴ@��!@���@��\@��+@�E�@�@�?}@��@��D@��F@��R@��@��h@�x�@�hs@�X@�%@��9@��D@�I�@���@�t�@�33@�@��H@��@���@�ȴ@�ȴ@���@���@��!@���@���@���@��\@��\@��+@�~�@�v�@�n�@�ff@�M�@�@��#@��h@���@��j@�Z@��@��
@���@�33@�n�@�X@���@�Q�@� �@���@���@�+@�V@��@��@��^@�hs@�Ĝ@�r�@��m@��@��F@���@�dZ@�@���@�{@��-@��h@��@�O�@��@�&�@�/@��@��@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AնFAոRAե�A�~�A�bNA�A�A��A���A��;A��yA��A���A�%A�  A��A��`A��#A��;A���A���A���A���AԴ9AԲ-AԮAԗ�AԁA�VAғuA�K�A�$�A�ffA�C�A��A��yA��uA��A�bA��9A��uA�&�A�A��A��\A���A��-A�7LA�VA��9A��A�oA���A��A�ĜA�
=A�E�A��+A�I�A�1'A�I�A��A���A�=qA�oA�A��HA�ZA��FA�%A�dZA�{A��A�bNA��yA�~�A�K�A��A��jA���A���A� �A�bA��A���A���A��A�VA�VA���A�A��A�x�A��TA�A��RA��RA���A���A��uA���A���A��FA���A�C�A���A�A��hA�"�A��A���A��A���A���A��A�5?A�bA��
A��FA}�A{x�Avn�ApjAm�PAl�Al$�Ajr�Ah�yAe�Aa�^A`n�A_%A^bA]�TA]�FA\�HA\A�AZQ�AX�AVffATffAS7LAR�DAQ��AQ�7AQ�APVANE�AM�PAJQ�AF�uADA�AA`BA?�A>r�A=O�A;��A:��A:jA:Q�A:�A9�TA97LA69XA3�wA1+A/�mA/A/�A.VA,ȴA*^5A)XA(^5A'��A'�hA'dZA&JA%&�A$�\A$$�A#ƨA#p�A#oA"Q�A!��A ��AAQ�A��A-A"�Az�A��A�A{A%A1A�jA��AA�-A7LAE�A�;A�wA�AA
�DA	��A�AC�A��AffA5?A�mAA�FA�A��A��A�hAt�A�A��A��AQ�A�yA�
A �/@�E�@�?}@�j@�1'@��@�@���@�Ĝ@��j@�I�@�v�@���@��@�S�@��^@�D@��@�^@�/@�ƨ@�\)@�+@�S�@�-@�  @�@݉7@��@�;d@��@ڧ�@�{@�z�@���@�"�@֟�@Ձ@�(�@�C�@҇+@�@љ�@��@�z�@�1'@���@�~�@�p�@�1'@�C�@��y@��@ɩ�@ɡ�@�x�@�7L@���@ȃ@�j@�b@ǥ�@ǅ@�;d@�
=@�v�@�@�V@ě�@�9X@�ƨ@�l�@���@�7L@��j@��u@�j@�(�@�ff@�5?@��T@�S�@�X@��m@���@���@�bN@��@�;d@�"�@��T@��@��@��w@��@�K�@��y@�=q@���@�~�@�~�@�5?@�@�7L@��@�%@�%@��@��u@�1'@���@�"�@�n�@�V@�-@��R@��@�1'@���@�t�@�C�@��@��@���@���@�E�@�%@�1@��w@��F@��@���@�@�V@��#@�x�@�p�@�X@�/@�V@���@��/@�Ĝ@��u@��@�1'@��w@���@�|�@�C�@�"�@�@�E�@��@�@�{@�@�?}@���@��@�j@�Z@�Q�@�(�@��@��;@�ƨ@���@��@�33@�ȴ@��!@���@��\@��+@�E�@�@�?}@��@��D@��F@��R@��@��h@�x�@�hs@�X@�%@��9@��D@�I�@���@�t�@�33@�@��H@��@���@�ȴ@�ȴ@���@���@��!@���@���@���@��\@��\@��+@�~�@�v�@�n�@�ff@�M�@�@��#@��h@���@��j@�Z@��@��
@���@�33@�n�@�X@���@�Q�@� �@���@���@�+@�V@��@��@��^@�hs@�Ĝ@�r�@��m@��@��F@���@�dZ@�@���@�{@��-@��h@��@�O�@��@�&�@�/@��@��@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�NB�TB�TB�`B�fB�yB�B�B�B��B��B��BBBBB  B  B��B��B��B��B��B��B��B��B��B�B�mB��B�Bx�B{�Br�Bn�Bm�Bp�Bq�Bq�Br�Bx�Bx�By�B~�B�B�B�%B�%B�1B�JB��B��B��B��B�B�'B�RB�XB�wBĜBĜBŢB��B��B��B��B��B��B�)B�yB�BBBBVB�B1'B<jBJ�B\)Bv�B|�B}�B}�B|�Bz�Bw�Bs�BiyB[#BM�B/BB��BhsB8RB"�B�B�dB��B}�B_;BJ�BB�B�BuBJB%BBBB  B
��B
�B
�NB
ȴB
��B
XB
8RB
 �B	��B	�
B	ŢB	��B	�dB	�B	��B	�JB	v�B	o�B	hsB	dZB	bNB	aHB	[#B	VB	K�B	A�B	8RB	1'B	-B	-B	+B	)�B	'�B	#�B	�B	�B	PB	B��B�B�`B�NB�5B�#B�B�
B�
B�B��B��BɺB�dB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�JB�1B�%B�+B�B�B� B}�B|�Bz�Bz�By�Bw�Bu�Bt�Bt�Bs�Br�Br�Bs�Br�Bq�Br�Br�Bo�Bm�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bl�Bl�Bk�BjBjBiyBhsBdZBdZBbNB_;B_;B_;B_;B^5B^5B]/B^5B^5B]/B\)B^5B_;B_;BbNBaHB_;B`BBiyBk�Bm�Bs�Bv�Bx�B�B�+B�1B�PB�{B��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�?B�9B�?B�?B�?B�LB�^B�qB��B��BƨBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�BB�BB�BB�;B�5B�ZB�TB�HB�sB�B�B�mB�TB�fB�yB�B�B��B	B	B	B	B	B	B	1B	\B	{B	{B	�B	�B	!�B	"�B	$�B	&�B	)�B	.B	/B	/B	1'B	6FB	9XB	@�B	G�B	P�B	T�B	W
B	XB	\)B	`BB	bNB	gmB	gmB	gmB	ffB	dZB	dZB	cTB	cTB	bNB	cTB	dZB	e`B	ffB	gmB	gmB	hsB	jB	k�B	jB	k�B	l�B	n�B	o�B	p�B	s�B	u�B	v�B	x�B	x�B	x�B	z�B	z�B	{�B	~�B	� B	�B	�B	�B	�B	�+B	�=B	�JB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�9B	�?B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�RB	�RB	�RB	�XB	�RB	�RB	�RB	�RB	�LB	�FB	�LB	�XB	�RB	�^B	�dB	�jB	�wB	�}B	��B	ÖB	ÖB	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�NB�TB�ZB�`B�fB�B�B�B�B��B��B��BBBBB  B  B��B��B��B��B��B��B��B��B��B��B�B�5B�1B�%B�1Bv�Bp�Bp�Bt�Bs�Br�Bt�By�By�B{�B�B�B�+B�+B�1B�DB�hB��B��B��B�B�!B�3B�XB�jB��BŢBŢBȴB��B��B��B��B��B�B�5B�B��BBBB\B�B2-B<jBI�B[#Bv�B}�B~�B~�B}�B{�Bx�Bu�Bl�B^5BR�B8RB�B�'Br�B<jB.BBŢB��B�1BhsBN�BO�B"�B�BVB%BBBBB
��B
��B
�mB
��B
�B
`BB
=qB
-B
DB	�5B	ǮB	ĜB	��B	�3B	�B	��B	{�B	t�B	k�B	e`B	cTB	dZB	]/B	\)B	R�B	G�B	>wB	5?B	/B	/B	,B	,B	+B	)�B	�B	�B	�B	1B��B�B�fB�`B�NB�/B�B�
B�B�
B�B�
B��BB�RB�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�oB�hB�hB�JB�7B�7B�B�B�B� B� B}�B|�Bz�Bz�Bw�Bu�Bt�Bu�Bs�Bs�Bs�Br�Bs�Bu�Br�Bq�Bn�Bn�Bn�Bn�Bm�Bl�Bl�Bl�Bl�Bl�Bl�Bk�Bk�BjBiyBiyBgmBffBe`BaHB`BB`BB_;BaHB^5B^5B^5B^5B_;BaHBbNBe`Be`BcTBe`Be`BjBm�Bn�Bs�Bv�Bz�B�B�7B�=B�\B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�9B�FB�?B�?B�FB�LB�XB�jB�wBBÖBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�5B�HB�BB�BB�BB�HB�ZB�ZB�`B�B�B�B�B�`B�mB�yB�B��B��B	B	B	B	B	%B	%B	1B	\B	{B	{B	�B	�B	!�B	"�B	$�B	&�B	)�B	/B	0!B	0!B	2-B	6FB	9XB	?}B	E�B	P�B	T�B	XB	XB	]/B	aHB	aHB	gmB	hsB	iyB	hsB	dZB	dZB	cTB	cTB	cTB	dZB	e`B	ffB	ffB	gmB	gmB	hsB	jB	k�B	jB	k�B	l�B	o�B	p�B	p�B	s�B	u�B	v�B	x�B	y�B	y�B	z�B	z�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�9B	�?B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�RB	�RB	�RB	�LB	�XB	�^B	�XB	�^B	�dB	�jB	�}B	��B	B	ÖB	ÖB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<D��<#�
<#�
<49X<�C�<49X<49X<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�1<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447342012010314473520120103144735  AO  ARGQ                                                                        20111130143056  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143056  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144735  IP                  G�O�G�O�G�O�                