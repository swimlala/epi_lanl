CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-26T07:00:53Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171026070053  20190604095308  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�0�!_�@1   @�0��DG�@:V�+J�c&E����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dx� D�fD�R�D�z�D��D� D�?
D�}qD��D�=D�ED�mqD��HD�3D�H D�x�D�RD��\D��D�W�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B2Q�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�D�%�D�a�D��=D��pD�\D�NfD���D��{D�!�D�T{D�|�D��D��D�W\DڈRD෮D���D�,)D�g
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��
A���A��yA���A���A�
=A�$�A�A�A�I�A�^5AɑhAɰ!AɾwAɾwA�ĜA�ȴA�ȴA�ȴA�ƨA�ĜA�ȴA���A���A���AɅAƩ�A�9XA��A�A���A�r�A��A��A��FA���A�E�A��TA�A���A�A�O�A�$�A�-A��/A�?}A��-A���A���A��mA�~�A�A�A��TA��A��DA��`A�G�A���A��
A�jA��RA��9A�dZA�ZA�(�A��jA�JA���A�JA�p�A�JA�I�A�n�A��yA�r�A�bA���A��A��A~�A}oA{�FAzbAy�Ax��Aw"�At��Ar~�AqO�ApQ�An�RAm�
Ak�mAi��Ah��AgdZAd�/Ac`BAb�AaoA_��A^�DA]�wA\bAZ�jAZjAY�
AX�AW�AWl�AU;dAS�7AQx�AO�;AO�AOS�AO
=AM�AK��AI�wAGhsAFffAE�#ADn�AB��AA�TAAC�A@�A@A?
=A=��A=`BA<��A<A�A;XA:��A9�#A8��A7��A7
=A6�A6�A5��A5VA4M�A4�DA3��A2I�A1��A1\)A1�A0�RA/��A.�`A.�DA,�9A+t�A+VA*E�A($�A'��A'S�A&��A&�!A&v�A&-A%�A$�A$$�A#�hA#VA"�\A"1'A!ƨA!l�A!%A��A�AVA��A�A�`Av�A+A�DA  AƨA�7A�A�+A�
A�\A��AK�A�A�A33A�AAC�A+A��A�#A\)AȴA�TA
�A
A	�7A�yAn�A��Ar�A�\A��AI�Ap�A ��@��9@�@���@�%@�r�@�o@���@�J@�?}@��u@�  @�\)@�V@��@�O�@���@��H@��@���@�@���@��@�x�@�9X@��@�I�@�hs@�l�@��@ާ�@��`@�$�@ؓu@��m@�+@�/@�I�@�ƨ@ҏ\@�@�/@�"�@�&�@���@��
@���@�?}@�j@ǅ@���@Ə\@�J@��@ēu@�ƨ@�+@�V@��h@��`@�(�@��@�dZ@�33@���@�E�@��@��-@�j@�o@�^5@�&�@��m@�l�@��R@�5?@�X@��u@��@�\)@��!@��#@���@�O�@���@�@��y@�ȴ@���@�{@���@�/@�A�@�ƨ@��R@�p�@��D@��D@�Q�@�A�@�C�@�x�@�?}@��#@���@�Z@��#@�+@�O�@���@��F@��H@���@�v�@���@�7L@��@�l�@���@��@�^5@��@�J@�-@���@�Ĝ@�Z@���@�S�@�@��@��R@�v�@�-@��h@���@��@�z�@�j@�Q�@�(�@�1@��m@�1@�I�@��m@���@�|�@�dZ@�33@��y@�$�@�@�O�@���@��j@�9X@��w@��P@�l�@�K�@�;d@�;d@�;d@�+@�33@�33@��@��y@��\@�5?@��@���@��h@���@�1'@���@�ƨ@�l�@�+@�l�@�|�@�\)@�
=@��!@�M�@���@���@�p�@�?}@��/@��@��@�Q�@���@�;d@��!@��+@��+@�M�@���@��7@��@���@��/@���@��@�r�@� �@�w@
=@~�+@~$�@}?}@|�j@|z�@|(�@{�
@{dZ@zn�@z-@zJ@zJ@y��@y��@y��@y�^@y�#@z^5@z�!@z�!@zM�@yhs@x��@xbN@xA�@xA�@x1'@xbN@y&�@xr�@x �@w��@w�w@w�P@w�@vȴ@vE�@u@u��@u/@t��@t�@t�@t�/@t�@tz�@tI�@t�@s��@sdZ@sC�@r�H@r�\@r=q@rJ@r�@q��@qG�@q7L@p��@n�'@f@�@`�[@[�;@X�o@T��@O��@I+@@ی@:($@1�j@-�@%*0@a@&�@V@t�@~�@U2@�`@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��/A��
A���A��yA���A���A�
=A�$�A�A�A�I�A�^5AɑhAɰ!AɾwAɾwA�ĜA�ȴA�ȴA�ȴA�ƨA�ĜA�ȴA���A���A���AɅAƩ�A�9XA��A�A���A�r�A��A��A��FA���A�E�A��TA�A���A�A�O�A�$�A�-A��/A�?}A��-A���A���A��mA�~�A�A�A��TA��A��DA��`A�G�A���A��
A�jA��RA��9A�dZA�ZA�(�A��jA�JA���A�JA�p�A�JA�I�A�n�A��yA�r�A�bA���A��A��A~�A}oA{�FAzbAy�Ax��Aw"�At��Ar~�AqO�ApQ�An�RAm�
Ak�mAi��Ah��AgdZAd�/Ac`BAb�AaoA_��A^�DA]�wA\bAZ�jAZjAY�
AX�AW�AWl�AU;dAS�7AQx�AO�;AO�AOS�AO
=AM�AK��AI�wAGhsAFffAE�#ADn�AB��AA�TAAC�A@�A@A?
=A=��A=`BA<��A<A�A;XA:��A9�#A8��A7��A7
=A6�A6�A5��A5VA4M�A4�DA3��A2I�A1��A1\)A1�A0�RA/��A.�`A.�DA,�9A+t�A+VA*E�A($�A'��A'S�A&��A&�!A&v�A&-A%�A$�A$$�A#�hA#VA"�\A"1'A!ƨA!l�A!%A��A�AVA��A�A�`Av�A+A�DA  AƨA�7A�A�+A�
A�\A��AK�A�A�A33A�AAC�A+A��A�#A\)AȴA�TA
�A
A	�7A�yAn�A��Ar�A�\A��AI�Ap�A ��@��9@�@���@�%@�r�@�o@���@�J@�?}@��u@�  @�\)@�V@��@�O�@���@��H@��@���@�@���@��@�x�@�9X@��@�I�@�hs@�l�@��@ާ�@��`@�$�@ؓu@��m@�+@�/@�I�@�ƨ@ҏ\@�@�/@�"�@�&�@���@��
@���@�?}@�j@ǅ@���@Ə\@�J@��@ēu@�ƨ@�+@�V@��h@��`@�(�@��@�dZ@�33@���@�E�@��@��-@�j@�o@�^5@�&�@��m@�l�@��R@�5?@�X@��u@��@�\)@��!@��#@���@�O�@���@�@��y@�ȴ@���@�{@���@�/@�A�@�ƨ@��R@�p�@��D@��D@�Q�@�A�@�C�@�x�@�?}@��#@���@�Z@��#@�+@�O�@���@��F@��H@���@�v�@���@�7L@��@�l�@���@��@�^5@��@�J@�-@���@�Ĝ@�Z@���@�S�@�@��@��R@�v�@�-@��h@���@��@�z�@�j@�Q�@�(�@�1@��m@�1@�I�@��m@���@�|�@�dZ@�33@��y@�$�@�@�O�@���@��j@�9X@��w@��P@�l�@�K�@�;d@�;d@�;d@�+@�33@�33@��@��y@��\@�5?@��@���@��h@���@�1'@���@�ƨ@�l�@�+@�l�@�|�@�\)@�
=@��!@�M�@���@���@�p�@�?}@��/@��@��@�Q�@���@�;d@��!@��+@��+@�M�@���@��7@��@���@��/@���@��@�r�@� �@�w@
=@~�+@~$�@}?}@|�j@|z�@|(�@{�
@{dZ@zn�@z-@zJ@zJ@y��@y��@y��@y�^@y�#@z^5@z�!@z�!@zM�@yhs@x��@xbN@xA�@xA�@x1'@xbN@y&�@xr�@x �@w��@w�w@w�P@w�@vȴ@vE�@u@u��@u/@t��@t�@t�@t�/@t�@tz�@tI�@t�@s��@sdZ@sC�@r�H@r�\@r=q@rJ@r�@q��@qG�@q7LG�O�@n�'@f@�@`�[@[�;@X�o@T��@O��@I+@@ی@:($@1�j@-�@%*0@a@&�@V@t�@~�@U2@�`@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBk�Bl�Bm�Bm�Bp�Br�Bz�B�7B��B��B��BÖB�B�TB�TB�B��B��B��BB\B�B&�B1'BA�BN�B6FB�B�-B�oB�1B� Bu�Bm�BhsBbNB]/BT�B;dB0!B%�B�BVB+BB��B�B�;BɺBĜB��B�}B�-B��B��B�{B�JB�By�Bq�B\)BO�BH�B;dB,B�BhBDBB
��B
�B
�fB
�
B
��B
B
�XB
��B
�%B
� B
v�B
hsB
\)B
N�B
E�B
>wB
1'B
�B
JB
B	��B	�B	�B	�;B	��B	ȴB	�wB	�B	��B	��B	�PB	�B	z�B	r�B	gmB	^5B	ZB	T�B	L�B	E�B	?}B	/B	 �B	bB	B	B	B	  B��B�B�;B��B��B��BƨB�}B�qB�qB�dB�^B�RB�3B�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�\B�VB�VB�VB�VB�VB�VB�JB�=B�7B�1B�B�B� B|�B|�Bu�Bs�Br�Br�Bm�BhsBe`BbNB`BB_;B^5B]/B\)BYBXBT�BQ�BQ�BO�BL�BI�BH�BG�BF�BE�BC�B@�B?}B>wB=qB<jB<jB;dB:^B8RB49B/B(�B%�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B"�B"�B"�B �B�B�B�B�B�B�B�B�B�B�B{BoBPBVBVBPB\BuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B$�B%�B'�B(�B)�B)�B+B,B,B,B/B33B49B8RB<jB=qB>wB?}BA�BD�BD�BF�BF�BH�BI�BJ�BN�BO�BO�BO�BO�BP�BVBZB]/B^5B_;B^5B_;B`BBcTBdZBcTB`BBe`Bk�Bm�Bn�Bk�BffBjBp�Br�By�B}�B~�B~�B�B�B�=B�JB�PB�bB�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�3B�RB�}BÖBŢBǮBǮBǮBɺB��B��B�B�B�B�B�#B�/B�5B�BB�BB�BB�HB�TB�mB�B�B��B��B��B��B��B��B��B	  B	B	B	B	%B	1B	
=B	DB	PB	bB	hB	uB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	,B	-B	-B	.B	0!B	2-B	49B	9XB	<jB	=qB	A�B	D�B	F�B	H�B	I�B	K�B	O�B	S�B	VB	VB	VB	VB	VB	ZB	_;B	e`B	gmB	iyB	jB	k�B	m�B	p�B	q�B	q�B	q�B	s�B	x�B	z�B	|�B	� B	�B	�B	�%B	�%B	�+B	�7B	�DB	�bB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�hB	��B
SB
�B
'�B
.IB
4�B
6�B
B�B
H1B
K�B
R�B
ZB
`�B
g�B
m�B
r|B
v�B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Bk[Bl`BmfBmfBp|Br�Bz�B�B�^B�yB��B�nB��B�.B�.B�ZB��B��B��B�B2B�B&�B0�BAcBN�B6B�pB�B�FB�B�Bu�BmcBhIBb"B]BT�B;7B/�B%�BcB)B�B�B��B�pB�BɋB�jB�\B�PB�B��B�|B�KB�B��By�Bq}B[�BO�BH�B;7B+�BqB9BB�B
��B
�B
�3B
��B
˖B
�^B
�'B
��B
��B
�B
v�B
hCB
[�B
N�B
EpB
>FB
0�B
sB
B
�B	��B	�~B	�^B	�B	ϫB	ȁB	�FB	��B	��B	�NB	�B	��B	z�B	r|B	g8B	^B	Y�B	T�B	L�B	EmB	?JB	.�B	 �B	.B	�B	�B	�B��B��B�KB�	B��BбB͝B�sB�HB�>B�=B�-B�)B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�[B�WB�KB�.B�(B�B�!B� B�B�B�B�B�B�B��B��B��B�B|�B|�Bu�Bs}BrxBrxBm[Bh<Be(BbB`	B_B]�B\�B[�BX�BW�BT�BQ�BQ�BO�BL�BI�BH{BGuBFlBEiBC\B@JB?DB>>B=7B<.B<1B;*B:%B8B4 B.�B(�B%�B!�B �BvBsBeBeBiBdBVBWBYBYBYBxB�B|B"�B#�B"�B"�B"�B �B|B}ByBpBeB_BRBDBJBKB@B4BBBBB!B:B5B?B=B?BCBUBOBOB[B]BbBjBwBvBtBBB �B!�B"�B$�B%�B'�B(�B)�B)�B*�B+�B+�B+�B.�B2�B3�B8B</B=2B>8B??BAKBDaBD^BFhBFlBHwBI|BJ�BN�BO�BO�BO�BO�BP�BU�BY�B\�B]�B^�B]�B^�B`BcBdBcB`Be#BkHBmSBnYBkDBf&BjABpdBrqBy�B}�B~�B~�B��B��B��B�	B�B�#B�*B�)B�1B�5B�?B�OB�nB�yB�B�~B�}B��B��B��B��B��B��B��B��B��B��B��B�B�AB�UB�bB�mB�oB�mB�}BϞBҲB��B��B��B��B��B��B��B�B�B�B�B�B�,B�YB�kB�~B��B��B��B��B��B��B��B	�B	�B	�B	�B	�B		�B	B	B	!B	(B	5B	6B	:B	<B	UB	aB	`B	bB	kB	xB	}B	B	}B	!�B	$�B	'�B	*�B	+�B	,�B	,�B	-�B	/�B	1�B	3�B	9B	<+B	=2B	AKB	D\B	FiB	HuB	I{B	K�B	O�B	S�B	U�B	U�B	U�B	U�B	U�B	Y�B	^�B	eB	g,B	i9B	j>B	kFB	mRB	pdB	qjB	qjB	qjB	svB	x�B	z�B	|�B	�B	��B	��B	��B	��B	��B	��B	�B	�"B	�.B	�5B	�5B	�5B	�;B	�BB	�DB	�LB	�[B	�_B	�^B	�eB	�xB	��B	��B	��B	��B	��B	��G�O�B	�~B	��B	�(B	�]B
B
�B
'IB
.	B
4�B
6�B
BOB
G�B
K�B
RB
Y�B
`iB
g�B
mmB
r?B
v�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953082019060409530820190604095308  AO  ARCAADJP                                                                    20171026070053    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171026070053  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171026070053  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095308  IP                  G�O�G�O�G�O�                