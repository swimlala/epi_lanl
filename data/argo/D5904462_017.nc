CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:17Z AOML 3.0 creation; 2016-08-07T21:51:11Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221417  20160807145112  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_017                   2C  D   APEX                            6529                            072314                          846 @�"���@1   @�"����@2��j~���c��
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D�	�D�&fD���D��3D�fD�6fD�y�D�� D���D�FfD��3D��fD�3D�VfD�|�D���D� D�,�D�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B�
=B��
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
B��
B��
B��
B��
B��
B��
B��
B�
=B���B��
B��
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�GDo�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�GDy�D�D�3�D��D��D��D�C�D��D��qD�D�S�D���D���D��D�c�Dڊ>D��>D�qD�:>D�D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aٝ�Aٟ�Aٕ�Aٕ�Aٙ�Aٙ�Aٕ�A٣�AپwA�%A�/A�G�A�^5A�ffA�hsA�l�A�n�A�p�A�hsA�^5A�ZA�Q�A�K�A�E�A�A�A�?}A�?}A�=qA�;dA�5?A�&�A�VAׁAգ�A�VAӾwA�ƨA��Aϥ�A�  A̼jA���A�JA�A��A+A�;dA�|�A���A��FA�G�A�
=A�z�A��A�t�A��;A��`A���A�A��\A�ZA�VA�A�jA�K�A��A�A�A�A��A�n�A��A��A�ȴA�%A��\A�S�A�oA�JA�ZA�hsA�JA���A��/A���A���A�I�A�ȴA�dZA�(�A�A�bNA���A���A�1A� �A�O�A�l�A�A��A�1A��A�S�A���A��jA�7LA~�yA}Ay�PAs��Ar�AoƨAm��Ak�Ai�^Ac�AbA`��A`��A]�TAY�AV��ATn�AQ�wAPJAN-AL9XAK�AJ��AI\)AGp�ACt�AB-A@�\A?A=S�A:bNA7?}A5�#A2E�A/hsA,��A,=qA+�A*�RA*1'A)l�A'�A'7LA'?}A& �A$�/A#�PA#7LA#oA"�yA"ȴA"n�A!l�A!A ĜA �yA �9A �\A ZAhsA�A7LAVAȴA�7A5?Al�A��A�yAQ�A{A��A;dA�uA��A�HAA�A�jAr�A��A�A��A�hA�A�/AZAS�A~�A��A
��A
ZA
(�A	�#A	�PA�+A��A��A��AA�-A��AA��A��Az�A�;Al�AĜA|�AS�A��A�\A�A ��@���@��/@�(�@�^5@��7@�?}@�j@�1@��@�C�@�~�@��@���@�O�@�\)@��H@�5?@���@�^@�@�~�@�R@��@��T@�@�j@� �@�%@ꟾ@�`B@�r�@��@���@�bN@�u@���@���@���@���@���@��m@���@�r�@�Z@�(�@��@�  @�dZ@�"�@�+@�$�@���@��#@���@�`B@�j@���@�\)@�+@�
=@���@�~�@�{@�7L@܋D@ۥ�@�J@�@�&�@�`B@�7L@ؓu@���@ו�@ׅ@�|�@�9X@�1'@׾w@��@���@�`B@�7L@��@��/@�bN@�1@�
=@ҏ\@�-@���@�@�`B@Ь@ЋD@�I�@�(�@�1@ύP@�
=@���@͑h@�O�@��@̼j@�j@��@ˮ@�t�@��H@�{@�@Ɂ@���@ȓu@���@�&�@���@�Q�@�I�@�Q�@��@ǍP@��H@�~�@�n�@Ɵ�@Ɵ�@Ƈ+@�5?@�x�@�A�@�"�@�C�@�+@��@��@���@�Ĝ@�9X@���@��w@���@�K�@��R@�M�@�{@��@�@��T@��#@���@��-@�x�@�hs@��7@���@�p�@�&�@�z�@�ƨ@�C�@�@��R@�=q@�{@��^@�`B@�/@��j@�Z@��@��F@�\)@�K�@���@�v�@��@�/@��@�9X@�1@��F@�o@��H@��@��@���@���@��!@�V@�@�X@�Ĝ@��D@��@���@�J@�@��-@���@�/@��j@�1'@��@��;@���@�dZ@���@�v�@�E�@��@���@�p�@���@�j@��@��F@�"�@�M�@�J@��-@��7@�7L@�r�@�(�@� �@�1@��m@�ƨ@��P@�"�@�ff@�{@��T@���@���@�Ĝ@�z�@�bN@�Q�@�1@��F@���@�t�@�+@��H@�~�@�=q@�{@���@��T@���@��h@�V@��@�Z@�(�@�1@���@��@�+@���@�M�@�E�@�@��@��@��@��u@�;d@��@���@�E�@}p�@vȴ@r=q@j�@aX@W�P@LZ@B�H@<�/@5/@,9X@%��@�P@��@5?@^5@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aٝ�Aٟ�Aٕ�Aٕ�Aٙ�Aٙ�Aٕ�A٣�AپwA�%A�/A�G�A�^5A�ffA�hsA�l�A�n�A�p�A�hsA�^5A�ZA�Q�A�K�A�E�A�A�A�?}A�?}A�=qA�;dA�5?A�&�A�VAׁAգ�A�VAӾwA�ƨA��Aϥ�A�  A̼jA���A�JA�A��A+A�;dA�|�A���A��FA�G�A�
=A�z�A��A�t�A��;A��`A���A�A��\A�ZA�VA�A�jA�K�A��A�A�A�A��A�n�A��A��A�ȴA�%A��\A�S�A�oA�JA�ZA�hsA�JA���A��/A���A���A�I�A�ȴA�dZA�(�A�A�bNA���A���A�1A� �A�O�A�l�A�A��A�1A��A�S�A���A��jA�7LA~�yA}Ay�PAs��Ar�AoƨAm��Ak�Ai�^Ac�AbA`��A`��A]�TAY�AV��ATn�AQ�wAPJAN-AL9XAK�AJ��AI\)AGp�ACt�AB-A@�\A?A=S�A:bNA7?}A5�#A2E�A/hsA,��A,=qA+�A*�RA*1'A)l�A'�A'7LA'?}A& �A$�/A#�PA#7LA#oA"�yA"ȴA"n�A!l�A!A ĜA �yA �9A �\A ZAhsA�A7LAVAȴA�7A5?Al�A��A�yAQ�A{A��A;dA�uA��A�HAA�A�jAr�A��A�A��A�hA�A�/AZAS�A~�A��A
��A
ZA
(�A	�#A	�PA�+A��A��A��AA�-A��AA��A��Az�A�;Al�AĜA|�AS�A��A�\A�A ��@���@��/@�(�@�^5@��7@�?}@�j@�1@��@�C�@�~�@��@���@�O�@�\)@��H@�5?@���@�^@�@�~�@�R@��@��T@�@�j@� �@�%@ꟾ@�`B@�r�@��@���@�bN@�u@���@���@���@���@���@��m@���@�r�@�Z@�(�@��@�  @�dZ@�"�@�+@�$�@���@��#@���@�`B@�j@���@�\)@�+@�
=@���@�~�@�{@�7L@܋D@ۥ�@�J@�@�&�@�`B@�7L@ؓu@���@ו�@ׅ@�|�@�9X@�1'@׾w@��@���@�`B@�7L@��@��/@�bN@�1@�
=@ҏ\@�-@���@�@�`B@Ь@ЋD@�I�@�(�@�1@ύP@�
=@���@͑h@�O�@��@̼j@�j@��@ˮ@�t�@��H@�{@�@Ɂ@���@ȓu@���@�&�@���@�Q�@�I�@�Q�@��@ǍP@��H@�~�@�n�@Ɵ�@Ɵ�@Ƈ+@�5?@�x�@�A�@�"�@�C�@�+@��@��@���@�Ĝ@�9X@���@��w@���@�K�@��R@�M�@�{@��@�@��T@��#@���@��-@�x�@�hs@��7@���@�p�@�&�@�z�@�ƨ@�C�@�@��R@�=q@�{@��^@�`B@�/@��j@�Z@��@��F@�\)@�K�@���@�v�@��@�/@��@�9X@�1@��F@�o@��H@��@��@���@���@��!@�V@�@�X@�Ĝ@��D@��@���@�J@�@��-@���@�/@��j@�1'@��@��;@���@�dZ@���@�v�@�E�@��@���@�p�@���@�j@��@��F@�"�@�M�@�J@��-@��7@�7L@�r�@�(�@� �@�1@��m@�ƨ@��P@�"�@�ff@�{@��T@���@���@�Ĝ@�z�@�bN@�Q�@�1@��F@���@�t�@�+@��H@�~�@�=q@�{@���@��T@���@��h@�V@��@�Z@�(�@�1@���@��@�+@���@�M�@�E�@�@��@��@��G�O�@�;d@��@���@�E�@}p�@vȴ@r=q@j�@aX@W�P@LZ@B�H@<�/@5/@,9X@%��@�P@��@5?@^5@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B!�B.B<jBiyB�B��B��B��B�B�B�B�B�!B�!B�'B�'B�'B�'B�'B�'B�'B�'B�'B�-B�3BȴB�sB1B�B�B+B9XBI�Bk�B|�B�1B�DB��B��B�-B�}BƨB��B�^B�?B�B�{Br�B\)BN�BXBiyB��B��B�1Bw�B|�B��BŢB��B��B�?B�BaHB,B�BDB��B��B�B�B�)B�B��B�;B�-B�B�B�B�B��B�bBy�Bo�BjBK�B-BB
��B
��B
�ZB
ɺB
�FB
�B
�7B
XB
�B	��B	�B	�TB	�B	ƨB	��B	��B	�\B	� B	s�B	jB	YB	P�B	J�B	K�B	I�B	9XB	,B	 �B	�B	PB	B��B��B�B�mB�BB�B�B�B��B�B�B��B��B��B��B��B��BɺBǮB��B��B�B�B�
B�ZB�mB�B�B�B�B�B�B�B��B	\B	�B	�B	�B	�B	�B	,B	8RB	7LB	49B	49B	49B	2-B	1'B	-B	+B	(�B	$�B	�B	!�B	�B	�B	�B	0!B	49B	2-B	2-B	6FB	8RB	9XB	9XB	7LB	8RB	9XB	@�B	=qB	?}B	@�B	>wB	;dB	33B	+B	B�B	D�B	K�B	K�B	J�B	J�B	N�B	N�B	O�B	T�B	VB	VB	Q�B	Q�B	T�B	YB	XB	M�B	G�B	L�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	P�B	Q�B	XB	bNB	_;B	]/B	^5B	^5B	_;B	aHB	ffB	n�B	q�B	r�B	p�B	s�B	u�B	x�B	n�B	k�B	p�B	r�B	x�B	|�B	�B	�%B	�DB	�bB	�oB	�uB	�oB	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�9B	�'B	�B	�B	�!B	�9B	�3B	�-B	�!B	�B	�!B	�!B	�^B	��B	ÖB	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�)B	�5B	�5B	�/B	�/B	�5B	�5B	�5B	�)B	�B	�B	�)B	�/B	�5B	�5B	�/B	�#B	�#B	�5B	�HB	�;B	�/B	�5B	�/B	�)B	�)B	�#B	�#B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�HB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�`B	�fB	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�`B	�`B	�`B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�fB	�mB	�mB	�sB	�yB	�B	�B	�yB	�mB	�`B	�fB	�fB	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B
B
1B
�B
�B
�B
#�B
&�B
-B
5?B
;dB
A�B
G�B
L�B
T�B
\)B
bNB
hsB
l�B
q�B
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B!�B-�B<IBiYB��B�iB��B��B��B��B��B��B�B� B�B�B�B�B�B�B�B�B�B�B�BȓB�RBBtB�B*�B97BI�BkcB|�B�B�$B��B��B�B�WBƆB�fB�;B�B��B�ZBr�B\BN�BW�BiWB�cB�vB�Bw�B|�B�nB�B��B��B�B��BaB+�BZBB��B��B�B�dB�B��B��B�B�B��B��B��B��B��B�9By�BovBjXBK�B,�B�B
��B
��B
�1B
ɔB
�B
��B
�B
W�B
�B	��B	�}B	�0B	��B	ƄB	��B	��B	�:B	�B	s�B	j`B	X�B	P�B	J�B	K�B	I�B	99B	+�B	 �B	cB	2B	B��B��B��B�QB�$B��B��B��B��B��B��B��B��B˨BλBͷB˪BɞBǏB��B��B��B��B��B�=B�MB�hB�cB�dB�eB�dB�eB�B��B	:B	kB	�B	�B	�B	�B	+�B	8-B	7(B	4B	4B	4B	2	B	1B	,�B	*�B	(�B	$�B	�B	!�B	�B	�B	�B	/�B	4B	2B	2	B	6!B	8.B	91B	91B	7)B	8.B	92B	@_B	=LB	?XB	@^B	>OB	;?B	3B	*�B	BjB	DwB	K�B	K�B	J�B	J�B	N�B	N�B	O�B	T�B	U�B	U�B	Q�B	Q�B	T�B	X�B	W�B	M�B	G�B	L�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	P�B	Q�B	W�B	b'B	_B	]B	^B	^B	_B	a!B	f=B	nmB	qB	r�B	p~B	s�B	u�B	x�B	noB	k\B	p}B	r�B	x�B	|�B	��B	��B	�B	�;B	�CB	�LB	�EB	�7B	�:B	�9B	�FB	�QB	�UB	�UB	�VB	�VB	�iB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�	B	�B	��B	��B	��B	��B	�1B	�^B	�gB	�tB	ǅB	ǁB	ǂB	ȈB	ȈB	ȆB	ɎB	˙B	̞B	ͧB	ΪB	ͧB	ίB	жB	зB	ѿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�(B	�#B	�(B	�'B	�&B	�+B	�,B	�-B	�0B	�8B	�2B	�8B	�3B	�+B	�-B	�-B	�*B	�0B	�0B	�7B	�2B	�2B	�3B	�6B	�9B	�2B	�3B	�3B	�6B	�5B	�6B	�?B	�7B	�>B	�>B	�DB	�NB	�NB	�QB	�LB	�@B	�1B	�6B	�8B	�6B	�=B	�LB	�KB	�LB	�OB	�WB	�QB	�OB	�XB	�\B	�cB	�aB	�aB	�cB	�aB	�eB	�dB	�^B	�iB	�jB	�mB	�qB	�pB	�uB	�wB	�wB	�zB	�{B	�yB	�zB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
 �B
B
VB
\B
{B
#�B
&�B
,�B
5B
;1B
AYB
G}B
L�B
T�B
[�B
bB
h>B
lYB
qwB
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451122016080714511220160807145112  AO  ARCAADJP                                                                    20150226221417    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221417  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221417  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145112  IP                  G�O�G�O�G�O�                