CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-16T00:03:15Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171016000315  20190604095308  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�(���R1   @�(�lw�B@9qhr� ��c?�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBR  BW33B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyaHD��
D�T�D�3�D���D�3D�A�D���D���D��D�4)D���D��\D��D�9�Dډ�D�θD�D�=qD� D�{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI��BS�\BXBa�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
\D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dyz>D��D�aHD�@ D��\D��D�NgD��qD��\D�D�@�D�� D���D�D�FgDږgD��3D��D�I�D�{D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A��yA��yA�ƨA֟�A֓uAօA�|�A�t�A�ffA�`BA�`BA�\)A�ZA�ZA�XA�VA�S�A�Q�A�Q�A�M�A�O�A�I�A�7LAԓuA��A��AuA��
A��yA�ĜA���A�n�A��#A�$�A�bA�&�A�33A��!A�l�A��A��A��wA��A�ȴA��A��A��wA�x�A��uA��\A���A�E�A�&�A�=qA�/A���A��wA�5?A��-A�A�A��jA��A�5?A��^A��A�5?A��9A�$�A�(�A�bA��\A�VA��PA�M�A��7A�33A�%A���A�%A��uA�bA��PA��PA���A�{A�dZA�l�A��#A�G�A�{AS�A~I�A}t�A|ȴA{XAz�9AyG�AvVAudZAt��As�-Ar�Aq&�Ao�
An��Am�FAm�AlZAj~�Ah�Ag��Af(�Ad �AbE�A`jA_C�A]�;A\ffA[�AY;dAWt�AV5?AU%AS�AR��AQAP9XAO\)AMALn�AKC�AH�DAH$�AGƨAG7LAFffADZAChsAB�ABM�AAl�A@��A@�jA@�A?33A>Q�A<ZA;oA:=qA9+A7�-A69XA5p�A4�uA3�mA2ȴA1�7A0ȴA/A/"�A.ĜA.$�A.  A-��A-7LA,�yA,^5A*��A*VA)��A)�A(��A(M�A(JA'x�A&z�A%�A%+A$Q�A#�#A#�-A#�A#33A"v�A!�^A!XA �A��A��A�AjA��A(�A�FA`BA;dA%A�DA��A��A�9A�AVAM�A��A��AG�A��A�A�#A�A?}AZA`BA�jAA�A�AoA
$�A	/Ar�A=qA�FA`BAr�A�^AG�AAn�AK�A��A=qAt�A&�A z�@�S�@�E�@�@�x�@���@��
@�o@�$�@� �@��^@�\@�%@�^5@��@�(�@�w@�V@�9@�dZ@�@��@���@���@�t�@�/@ە�@ڸR@��T@ج@�+@�hs@���@��@��@�@��m@�x�@̛�@��@˾w@�\)@�~�@��@�ƨ@�|�@�$�@�I�@�S�@���@��D@���@��H@���@���@�"�@��!@�@��@�  @��@��!@�E�@�7L@��@���@��@�ff@���@�ȴ@�=q@���@�A�@���@��@�`B@�7L@�&�@�V@���@�A�@��@�ff@�`B@���@�Q�@�  @�dZ@��H@�M�@�?}@�1'@��@�t�@�t�@�\)@�"�@�o@��@�v�@��h@��@�9X@��@��@�|�@�t�@�l�@�S�@�{@���@�p�@�O�@��`@��j@��j@���@���@�t�@�K�@��@���@�J@�p�@��@��/@�1'@�l�@�o@���@�V@���@��7@�Ĝ@�Q�@���@���@�@�ȴ@��!@���@��+@�~�@�n�@�V@��@��h@�Ĝ@�j@�9X@��m@�\)@���@�{@�hs@�V@��`@��j@�j@��@�  @���@���@���@�C�@���@�v�@�$�@�X@��@��@��/@��j@�bN@�(�@��F@�"�@�5?@��^@��-@�@���@�O�@��@��@���@���@��m@�
=@��@�ȴ@��!@�^5@�@��^@���@��h@�/@��u@�b@�w@|�@l�@;d@+@~�y@~ȴ@~�+@~{@}�T@}�-@}�@}O�@|z�@{C�@z��@z=q@z=q@y��@y�^@yG�@y�@x�@xA�@w�;@wl�@w�@v�@w
=@w+@v�@vE�@u�h@tI�@s"�@r~�@rJ@q�^@q�7@qX@q7L@q&�@pĜ@p�9@p�`@p��@pĜ@pQ�@p1'@p �@pb@o�;@o�@k��@e�9@`bN@Y�@U��@P�U@Kv`@C�
@=��@4$@.�c@)��@#��@!@@ �@��@  @��@ѷ@�e@C-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�A��yA��yA�ƨA֟�A֓uAօA�|�A�t�A�ffA�`BA�`BA�\)A�ZA�ZA�XA�VA�S�A�Q�A�Q�A�M�A�O�A�I�A�7LAԓuA��A��AuA��
A��yA�ĜA���A�n�A��#A�$�A�bA�&�A�33A��!A�l�A��A��A��wA��A�ȴA��A��A��wA�x�A��uA��\A���A�E�A�&�A�=qA�/A���A��wA�5?A��-A�A�A��jA��A�5?A��^A��A�5?A��9A�$�A�(�A�bA��\A�VA��PA�M�A��7A�33A�%A���A�%A��uA�bA��PA��PA���A�{A�dZA�l�A��#A�G�A�{AS�A~I�A}t�A|ȴA{XAz�9AyG�AvVAudZAt��As�-Ar�Aq&�Ao�
An��Am�FAm�AlZAj~�Ah�Ag��Af(�Ad �AbE�A`jA_C�A]�;A\ffA[�AY;dAWt�AV5?AU%AS�AR��AQAP9XAO\)AMALn�AKC�AH�DAH$�AGƨAG7LAFffADZAChsAB�ABM�AAl�A@��A@�jA@�A?33A>Q�A<ZA;oA:=qA9+A7�-A69XA5p�A4�uA3�mA2ȴA1�7A0ȴA/A/"�A.ĜA.$�A.  A-��A-7LA,�yA,^5A*��A*VA)��A)�A(��A(M�A(JA'x�A&z�A%�A%+A$Q�A#�#A#�-A#�A#33A"v�A!�^A!XA �A��A��A�AjA��A(�A�FA`BA;dA%A�DA��A��A�9A�AVAM�A��A��AG�A��A�A�#A�A?}AZA`BA�jAA�A�AoA
$�A	/Ar�A=qA�FA`BAr�A�^AG�AAn�AK�A��A=qAt�A&�A z�@�S�@�E�@�@�x�@���@��
@�o@�$�@� �@��^@�\@�%@�^5@��@�(�@�w@�V@�9@�dZ@�@��@���@���@�t�@�/@ە�@ڸR@��T@ج@�+@�hs@���@��@��@�@��m@�x�@̛�@��@˾w@�\)@�~�@��@�ƨ@�|�@�$�@�I�@�S�@���@��D@���@��H@���@���@�"�@��!@�@��@�  @��@��!@�E�@�7L@��@���@��@�ff@���@�ȴ@�=q@���@�A�@���@��@�`B@�7L@�&�@�V@���@�A�@��@�ff@�`B@���@�Q�@�  @�dZ@��H@�M�@�?}@�1'@��@�t�@�t�@�\)@�"�@�o@��@�v�@��h@��@�9X@��@��@�|�@�t�@�l�@�S�@�{@���@�p�@�O�@��`@��j@��j@���@���@�t�@�K�@��@���@�J@�p�@��@��/@�1'@�l�@�o@���@�V@���@��7@�Ĝ@�Q�@���@���@�@�ȴ@��!@���@��+@�~�@�n�@�V@��@��h@�Ĝ@�j@�9X@��m@�\)@���@�{@�hs@�V@��`@��j@�j@��@�  @���@���@���@�C�@���@�v�@�$�@�X@��@��@��/@��j@�bN@�(�@��F@�"�@�5?@��^@��-@�@���@�O�@��@��@���@���@��m@�
=@��@�ȴ@��!@�^5@�@��^@���@��h@�/@��u@�b@�w@|�@l�@;d@+@~�y@~ȴ@~�+@~{@}�T@}�-@}�@}O�@|z�@{C�@z��@z=q@z=q@y��@y�^@yG�@y�@x�@xA�@w�;@wl�@w�@v�@w
=@w+@v�@vE�@u�h@tI�@s"�@r~�@rJ@q�^@q�7@qX@q7L@q&�@pĜ@p�9@p�`@p��@pĜ@pQ�@p1'@p �@pb@o�;G�O�@k��@e�9@`bN@Y�@U��@P�U@Kv`@C�
@=��@4$@.�c@)��@#��@!@@ �@��@  @��@ѷ@�e@C-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB=qB>wB?}B@�B@�BA�BA�BA�BB�BC�BC�BC�BE�BE�BE�BF�BE�BE�BF�BF�BF�BF�B>wB:^BJBB��B�B�`B�/B��B��BɺBB�3B��B��B�VB�Bq�Bs�Bn�BhsBffBe`BcTB]/BS�BH�B@�B9XB.B"�BuBB��B��B�B�B�ZB�)B��BȴB�qB�'B��B�bB�B}�Bs�B`BBR�B?}B2-B+B'�B#�B�BbBJBB
�B
�HB
�B
ǮB
�?B
��B
�B
o�B
ffB
]/B
T�B
N�B
C�B
=qB
1'B
�B
oB
JB
B	��B	�B	�BB	�B	��B	ȴB	B	�?B	��B	��B	�hB	�B	r�B	e`B	ZB	P�B	E�B	9XB	-B	"�B	�B	hB	
=B	B��B��B��B	B��B�B�5B�TB�BB�/B�B��B��B��BȴBȴBƨBĜB��B�jB�?B�B��B��B��B��B�uB�oB�hB�bB�VB�DB�=B�JB�JB�DB�=B�7B�1B�1B�+B�B�B�B�B�B�B�B�B�B~�B}�B|�B|�B|�B}�B}�B{�B{�B{�Bz�Bw�Bt�Bl�BhsBjBffBaHBbNBaHB`BB_;B\)BYBVBS�BP�BM�BK�BG�BD�BC�BA�B@�B>wB>wB<jB9XB9XB7LB6FB5?B2-B0!B.B.B-B,B+B(�B(�B'�B&�B#�B"�B"�B�B�B�B#�B!�B�B�B�B�B�B�B�B�B�B�B�BhB\BVBDBDB
=B	7B+B+BBBBBBBB  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBB%BB+B+B+B	7BDBDBPBPB\BuB�B�B&�B0!B7LB9XB7LB7LB8RB<jB>wB?}B?}B?}B?}BA�BC�BC�BD�BH�BK�BO�BP�BP�BR�BW
B\)B`BBaHBbNBe`BgmBl�Bo�Bq�Bp�Br�Bu�Bz�B|�B}�B~�B� B~�B� B�B�B�%B�7B�DB�VB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�FB�RB�dB�dB��B��B��BBBBÖBÖBĜBǮB��B��B��B��B��B��B�B�HB�`B�sB�yB�B�B�B�B�B�B�B��B��B��B	B	%B	1B		7B		7B	JB	VB	bB	oB	{B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	&�B	)�B	+B	,B	-B	/B	2-B	33B	33B	5?B	8RB	:^B	=qB	>wB	@�B	@�B	A�B	B�B	C�B	C�B	E�B	G�B	H�B	H�B	I�B	J�B	M�B	P�B	R�B	S�B	VB	XB	XB	YB	ZB	]/B	`BB	dZB	e`B	e`B	e`B	gmB	jB	m�B	p�B	r�B	s�B	s�B	t�B	w�B	x�B	y�B	y�B	z�B	z�B	}�B	� B	�B	�+B	�7B	�JB	�JB	�JB	�PB	�PB	�\B	��B	�9B	��B	��B
^B
�B
!�B
+�B
2�B
>�B
F�B
MjB
T�B
WsB
[�B
bB
h
B
l�B
p�B
v�B
|P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B>XB>WB=QB>YB?^B@dB@gBAhBAhBAjBBnBCwBCwBCwBE�BE�BE�BF�BE�BE�BF�BF�BF�BF�B>UB:AB)B�B��B�lB�?B�B��BϻBɗB�nB�B��B��B�4B��Bq�Bs�BnvBhNBfBBe?Bc/B]BS�BH�B@]B93B-�B"�BOB�B��B��B��B�[B�5B�BпBȑB�NB�B��B�=B��B}�Bs�B`BR�B?TB2
B*�B'�B#�BeB8B'B�B
�B
�"B
��B
ǉB
�B
�B
��B
ouB
f=B
]B
T�B
N�B
CnB
=KB
1B
}B
HB
$B
�B	��B	�VB	�B	��B	ͫB	ȍB	�hB	�B	��B	��B	�?B	��B	r�B	e9B	Y�B	P�B	EwB	9-B	,�B	"�B	qB	?B	
B	�B��B��B��B	 �B��B�fB�
B�+B�B�B��B��BͨBʖBȈBȈB�{B�pB�`B�>B�B��B��B��B�hB�jB�JB�EB�;B�6B�)B�B�B� B�B�B�B�B�B�B� B��B��B��B��B��B��B��B��B��B~�B}�B|�B|�B|�B}�B}�B{�B{�B{�Bz�Bw�Bt�Bl]BhEBjRBf9BaBb"BaB`B_B[�BX�BU�BS�BP�BM�BK�BG�BDnBCgBAXB@SB>FB>HB<?B9)B9(B7 B6B5B2 B/�B-�B-�B,�B+�B*�B(�B(�B'�B&�B#�B"�B"�B�B�B�B#�B!�B�B�B�B�B�B�B�BpBbB\BRB5B)B$BBB
B	B�B�B�B�B�B�B�B�B �B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B �B�B�B�B�B�B�B�B	BBBBB*BCBUBmB&�B/�B7B9&B7B7B8 B<6B>FB?IB?IB?JB?HBAVBCcBCcBDhBH�BK�BO�BP�BP�BR�BV�B[�B`BaBbBe/Bg;BlVBojBqtBprBrzBu�Bz�B|�B}�B~�B�B~�B�B��B��B��B�B�B�!B�@B�XB�`B�_B�gB�yB��B��B��B��B��B��B��B��B��B��B��B�B�B�1B�2B�PB�TB�UB�^B�]B�]B�bB�`B�hB�zB̗BϬBбBбBѻB��B��B�B�+B�?B�FB�OB�eB�jB�lB�pB�wB�B��B��B��B	�B	�B	�B		B		B	B	$B	.B	;B	HB	YB	_B	xB	�B	�B	!�B	"�B	#�B	%�B	&�B	)�B	*�B	+�B	,�B	.�B	1�B	2�B	2�B	5
B	8!B	:+B	=@B	>EB	@QB	@OB	AUB	BXB	CcB	CcB	EnB	GzB	H~B	H�B	I�B	J�B	M�B	P�B	R�B	S�B	U�B	W�B	W�B	X�B	Y�B	\�B	`B	d'B	e+B	e+B	e+B	g8B	jHB	m\B	prB	r}B	s�B	s�B	t�B	w�B	x�B	y�B	y�B	z�B	z�B	}�B	�B	��B	��B	�B	�B	�B	�B	�B	�G�O�B	��B	�B	�B	��B
*B
�B
!�B
+�B
2dB
>uB
F�B
M6B
T�B
W@B
[�B
a�B
g�B
lsB
p�B
v�B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953082019060409530820190604095308  AO  ARCAADJP                                                                    20171016000315    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171016000315  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171016000315  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095308  IP                  G�O�G�O�G�O�                