CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:09Z AOML 3.0 creation; 2016-08-07T21:51:10Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221409  20160807145110  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               
A   AO  5287_9017_010                   2C  D   APEX                            6529                            072314                          846 @�R�Z�1   @�S\���@2%�����c�dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    
A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C�fC�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D�	�D�I�D�vfD��fD��D�VfD�vfD�� D��D�I�D�vfD���D� D�L�D�vfD�ɚD�  D�33D�i�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
=Ḅ�B��
B��
Bأ�B��
B��
B��
B��
B��
B�
=B��
B��
B��
C k�CQ�CQ�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CD�CF�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D!GD��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�D�D�WD���D���D�>D�c�D���D��qD�>D�WD���D��>D�qD�Z>Dڃ�D��D�qD�@�D�wD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A޺^A�Q�A�(�A��A��A��A�
=A�%A�  A���A���A��A��A��A��A��A��A��A��A��A��`A��/A���A���A�ƨA���AݾwAݼjAݕ�A�bNA�G�A�$�A�hsA���A��#AہA���Aڏ\A�p�A��#A���A؟�A׃A�A�AՇ+A�v�A��HAґhA�jAБhA��#AͼjA���A�XA�5?A��AöFA��\A���A��A�"�A�bNA���A�A�A�v�A�XA�A��RA�O�A��
A���A�A�|�A�l�A�+A��A�Q�A���A�/A���A��!A���A�5?A��A�+A��A���A�&�A�33A��jA� �A�K�A|��Ay�Aw33AtZAlv�Aj$�Ae�PAb(�A_x�AZ�AW?}ASC�ARn�AP�RAO��AO?}AN�yAM��AL�ALn�AK�mAKS�AJ=qAH�`AF��AD��AC�A?�A>jA< �A:I�A7�A4v�A3��A2��A2��A0��A/&�A-%A+C�A)�#A((�A'��A'
=A&n�A%��A$z�A#t�A"bNA!33A 1'A�`AjAI�A��A(�A5?AbNA�yA�A��Az�A��A�7AbNA|�A`BAI�AJA�TA��A�FA�A	�A��A��A��A?}A�yA�!A�+Ar�AQ�A{AhsA�A�RAr�AE�A�FAv�A�A��A��AXA v�@��P@��@���@��+@�-@��^@�O�@�&�@��@�7L@�&�@�V@��@�9X@��;@�  @��w@��@�S�@��@���@��@�33@��!@�=q@�@��D@���@��-@��@�r�@��@�l�@�@�ȴ@��@���@��@��D@�r�@�b@��
@�F@�t�@��@�ȴ@�E�@��@���@�?}@�%@���@���@�j@�D@� �@�
=@�~�@�^5@���@�7@�-@�r�@��@�+@���@�7L@�Ĝ@�r�@�u@��/@�/@��/@�F@�R@���@���@���@ߕ�@�o@ޗ�@���@��@�Ĝ@� �@�+@ڏ\@��@���@٩�@�G�@ش9@��
@�o@�-@��#@պ^@ա�@��@�j@�b@�33@�n�@���@�`B@���@д9@�z�@�ƨ@�o@��H@Η�@�=q@��@͡�@���@˝�@ʰ!@�~�@�V@�p�@�%@ȼj@�r�@��@��
@ǥ�@�t�@�K�@��@���@�~�@Ų-@��;@öF@å�@ÍP@�dZ@�33@�+@�"�@��@�o@���@��y@°!@�@�V@�Z@��P@���@�ȴ@��+@�=q@�J@���@��^@��@���@��
@�o@��+@�n�@�^5@�E�@�J@��@�X@�%@���@��u@�A�@���@��F@�C�@��H@���@�M�@��@�@��@���@��-@���@���@��@�&�@��`@�9X@�ƨ@�33@���@�M�@�=q@�J@�@���@�Z@�9X@�(�@���@���@�\)@��@���@�v�@�^5@��@�@��@���@�Q�@���@�S�@�
=@��H@�ȴ@���@���@��\@��+@�E�@���@�hs@�?}@��@��9@��@�33@�ȴ@��+@��@�@���@�X@��@��j@�j@��@���@�C�@�+@��H@�ȴ@�n�@���@�p�@���@���@�Ĝ@��D@�j@�A�@��@��@�C�@��+@�^5@�@�hs@�7L@��/@��D@��w@�
=@���@�5?@���@��@���@�j@�A�@��;@�dZ@�+@�@��!@�v�@�$�@���@�O�@���@���@���@��j@��@��@�1'@�t�@��@�
=@��y@���@���@���@�~�@�V@�M�@�M�@�=q@�5?@��h@��h@�?}@z�@q7L@d�@Z=q@R=q@G�@@��@6�R@-�-@*�@%�@  �@�/@Ĝ@��@�P@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A޺^A�Q�A�(�A��A��A��A�
=A�%A�  A���A���A��A��A��A��A��A��A��A��A��A��`A��/A���A���A�ƨA���AݾwAݼjAݕ�A�bNA�G�A�$�A�hsA���A��#AہA���Aڏ\A�p�A��#A���A؟�A׃A�A�AՇ+A�v�A��HAґhA�jAБhA��#AͼjA���A�XA�5?A��AöFA��\A���A��A�"�A�bNA���A�A�A�v�A�XA�A��RA�O�A��
A���A�A�|�A�l�A�+A��A�Q�A���A�/A���A��!A���A�5?A��A�+A��A���A�&�A�33A��jA� �A�K�A|��Ay�Aw33AtZAlv�Aj$�Ae�PAb(�A_x�AZ�AW?}ASC�ARn�AP�RAO��AO?}AN�yAM��AL�ALn�AK�mAKS�AJ=qAH�`AF��AD��AC�A?�A>jA< �A:I�A7�A4v�A3��A2��A2��A0��A/&�A-%A+C�A)�#A((�A'��A'
=A&n�A%��A$z�A#t�A"bNA!33A 1'A�`AjAI�A��A(�A5?AbNA�yA�A��Az�A��A�7AbNA|�A`BAI�AJA�TA��A�FA�A	�A��A��A��A?}A�yA�!A�+Ar�AQ�A{AhsA�A�RAr�AE�A�FAv�A�A��A��AXA v�@��P@��@���@��+@�-@��^@�O�@�&�@��@�7L@�&�@�V@��@�9X@��;@�  @��w@��@�S�@��@���@��@�33@��!@�=q@�@��D@���@��-@��@�r�@��@�l�@�@�ȴ@��@���@��@��D@�r�@�b@��
@�F@�t�@��@�ȴ@�E�@��@���@�?}@�%@���@���@�j@�D@� �@�
=@�~�@�^5@���@�7@�-@�r�@��@�+@���@�7L@�Ĝ@�r�@�u@��/@�/@��/@�F@�R@���@���@���@ߕ�@�o@ޗ�@���@��@�Ĝ@� �@�+@ڏ\@��@���@٩�@�G�@ش9@��
@�o@�-@��#@պ^@ա�@��@�j@�b@�33@�n�@���@�`B@���@д9@�z�@�ƨ@�o@��H@Η�@�=q@��@͡�@���@˝�@ʰ!@�~�@�V@�p�@�%@ȼj@�r�@��@��
@ǥ�@�t�@�K�@��@���@�~�@Ų-@��;@öF@å�@ÍP@�dZ@�33@�+@�"�@��@�o@���@��y@°!@�@�V@�Z@��P@���@�ȴ@��+@�=q@�J@���@��^@��@���@��
@�o@��+@�n�@�^5@�E�@�J@��@�X@�%@���@��u@�A�@���@��F@�C�@��H@���@�M�@��@�@��@���@��-@���@���@��@�&�@��`@�9X@�ƨ@�33@���@�M�@�=q@�J@�@���@�Z@�9X@�(�@���@���@�\)@��@���@�v�@�^5@��@�@��@���@�Q�@���@�S�@�
=@��H@�ȴ@���@���@��\@��+@�E�@���@�hs@�?}@��@��9@��@�33@�ȴ@��+@��@�@���@�X@��@��j@�j@��@���@�C�@�+@��H@�ȴ@�n�@���@�p�@���@���@�Ĝ@��D@�j@�A�@��@��@�C�@��+@�^5@�@�hs@�7L@��/@��D@��w@�
=@���@�5?@���@��@���@�j@�A�@��;@�dZ@�+@�@��!@�v�@�$�@���@�O�@���@���@���@��j@��@��@�1'@�t�@��@�
=@��y@���@���@���@�~�@�V@�M�@�M�G�O�@�5?@��h@��h@�?}@z�@q7L@d�@Z=q@R=q@G�@@��@6�R@-�-@*�@%�@  �@�/@Ĝ@��@�P@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�
B
�`B
�fB
�sB
�B
��B
��BBBBBB%B+B1B
=BJBVBoB�B�B!�B)�B-B.B/B0!B1'B1'B1'B0!B/B/B.B2-B7LB,B �B�BoBB
��B
��B
�B
�mB
�;B
�5B
�NB
�sB
�B1BVBuB@�B|�B��B�RB��BǮBB�dB�FB�LB�}B�qB�9B�B��B��B��B��B��B�uB�+B}�Bw�Bl�B_;BS�BR�BO�B=qB'�B�B
�B
��B
��B
l�B
]/B
/B
1B	�
B	�qB	��B	��B	�B	[#B	K�B	33B	 �B	hB��B�B�B�yB�B�B�B�yB�B�B�B�B�sB�mB�mB�B�B�B�mB�NB�)B�B��B��B��B��B��B��B��B��B�B�;B�TB�NB�HB�BB�BB�;B�/B�/B�5B�BB�TB�ZB�TB�HB�BB�B��BǮBBÖB��B�B��B�B��B��BɺBɺBȴBȴBǮBƨBŢBŢBƨBƨBƨBŢBŢBŢBŢBĜBĜBĜBŢBƨBƨBƨBŢBȴB��B��B�
B�
B��B��B�B�
B�B�5B�TB�yB�B��B��B��B��B	B	DB	�B	�B	�B	!�B	&�B	)�B	49B	>wB	H�B	L�B	K�B	I�B	K�B	O�B	Q�B	Q�B	P�B	O�B	M�B	M�B	Q�B	S�B	VB	XB	^5B	`BB	bNB	e`B	hsB	k�B	m�B	p�B	s�B	t�B	u�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�1B	�1B	�7B	�JB	�\B	�PB	�VB	�VB	�PB	�PB	�JB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�bB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�-B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	�}B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
1B
bB
�B
"�B
(�B
49B
;dB
>wB
>wB
E�B
J�B
P�B
YB
^5B
bNB
e`B
iyB
n�B
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
�CB
�GB
�UB
�B
��B
��B�B�B�B�B�BBBB
B-B5BSBmB�B!�B)�B,�B-�B.�B0B1
B1B1B0B.�B.�B-�B2B7,B+�B �B�BMB�B
��B
��B
�uB
�PB
�B
�B
�.B
�TB
�dBB:BWB@cB|�B��B�,BϻBǌB�mB�AB�!B�(B�[B�MB�B��B��B��B��B��B�pB�OB�B}�Bw�BlfB_BS�BR�BO�B=KB'�B_B
�B
��B
��B
leB
]
B
.�B
B	��B	�TB	��B	�pB	��B	[B	K�B	3B	 �B	NB��B��B�dB�`B�bB�`B�dB�^B�dB�eB�cB�cB�YB�PB�SB�qB�jB�bB�QB�2B�B��B��B��B��B��B��BͷBλB��B��B�B�6B�0B�)B�"B�%B�B�B�B�B�$B�7B�;B�4B�)B�!B��B��BǍB�oB�wB˨B��B��B��B��B˦BɛBɘBȕBȓBǋBƈBłBŁBƇBƇBƄBŀBŀBŁBŃB�zB�}B�|BŃBƈBƅBƅBŁBȒBʢB��B��B��B��B��B��B��B��B�B�1B�VB��B��B��B��B��B	�B	B	bB	�B	�B	!�B	&�B	)�B	4B	>QB	H�B	L�B	K�B	I�B	K�B	O�B	Q�B	Q�B	P�B	O�B	M�B	M�B	Q�B	S�B	U�B	W�B	^B	`B	b%B	e8B	hLB	k^B	mjB	p{B	s�B	t�B	u�B	|�B	~�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�3B	�(B	�+B	�.B	�(B	�(B	�B	� B	�8B	�\B	�zB	��B	��B	�zB	�aB	�UB	�[B	�[B	�WB	�KB	�:B	�2B	�9B	�CB	�UB	�\B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�$B	�+B	�1B	�;B	�=B	�FB	�HB	�IB	�RB	�iB	�oB	�vB	�tB	�uB	�vB	�vB	�zB	�zB	�zB	�{B	�{B	�vB	ǂB	ȆB	ɍB	ʓB	˚B	˜B	̞B	̟B	̞B	̡B	̠B	̠B	ͦB	ΪB	жB	ѿB	��B	ѾB	��B	ѿB	ѾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�%B	�%B	�$B	�%B	�&B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�B	�%B	�'B	�&B	�,B	�)B	�+B	�-B	�-B	�8B	�CB	�JB	�KB	�LB	�JB	�VB	�cB	�bB	�iB	�vB	�wB	�}B	�|B	�{B	�|B	�yB	�zB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�G�O�B
B
3B
]B
"�B
(�B
4	B
;4B
>FB
>EB
EnB
J�B
P�B
X�B
^B
bB
e-B
iGB
nfB
qwB
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451102016080714511020160807145110  AO  ARCAADJP                                                                    20150226221409    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221409  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221409  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145110  IP                  G�O�G�O�G�O�                