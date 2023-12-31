CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-29T09:23:30Z AOML 3.0 creation; 2016-08-07T21:51:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160629092330  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_130                   2C  D   APEX                            6529                            072314                          846 @׷�\(��1   @׷���d@1-V�d�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C�C  C��C�fC  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dys3D���D���D��fD�� D�fD�P D�� D���D��D�<�D���D��3D�3D�P DچfD���D��D�6fD�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B�(�B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C z�C�{Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�C�{Cz�C{CaGCz�Cz�C aGC"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*�D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DtxRDy��D��D��)D���D��\D�%�D�_\D��\D���D�(�D�L)D��)D��D�"�D�_\Dڕ�D��)D�)D�E�D�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�JA���Aڝ�Aڟ�A�l�A�5?A�E�A�I�A�5?A�(�A�JA�JA�oA���A��A��`A��#AٶFA�~�A�~�AفAفA�~�AفAه+AًDAى7AًDAًDAمAمAًDAٍPAًDAًDAُ\Aٗ�Aٝ�Aٙ�Aٟ�A־wA�`BA�p�A��A�G�A�n�A�|�A�O�A�A�$�A��mA̲-A�p�A�VA�9XA��TA˴9A�n�A��
AɃA�  AȾwAȏ\A�\)Aƕ�A�ZA�ZA���A�?}A�ffA���A�bNA�z�A�ȴA�(�A�oA��A��A��`A�ƨA��\A�A�(�A��FA��
A�XA��A�K�A�9XA�t�A��A��hA�$�A���A�E�A���A���A�K�A�\)A�M�A�`BA�1'A�A�{A��DA��A���A���A��!A�VA��A���A|�Ay��Ax{Aw�As�mAq+An��AmVAk��Aj�DAj  Ai7LAg7LAe��Ac�PA\ffAY%AX-AV^5AR�/AOhsAOVANjAM�;AM�7AL�`AJ�`AE�;AD-AC+AB-A@v�A>I�A;
=A7G�A3��A3S�A1l�A0A-�;A-�A,ZA+�7A(n�A'�PA&�yA&$�A$�+A"M�A ��A Q�A��A�wAhsA/A�DA?}AjA��AM�AȴA�uA~�A^5AffA`BA��A�Ap�A��AE�AA��A��A��AVA��AC�Az�A��A �A��A
�A��A�A`BA(�A\)A��A��AI�A�^A;dAoA �9A z�A ^5A M�A 1@�x�@�V@�G�@�t�@�&�@�r�@�bN@�A�@�@�\@�n�@�E�@�v�@��@��@��
@�  @�|�@�$�@�-@�h@�&�@���@��m@�R@�V@���@�V@��@�+@�hs@� �@�Q�@�P@�@�+@�"�@�  @�@� �@�r�@�bN@�ƨ@�t�@�o@�!@�ff@�7L@��@�@�"�@�ff@�$�@�/@�j@��
@��@ް!@�{@ݡ�@��@܋D@��@�dZ@�"�@ڟ�@��@�J@��@١�@���@�1'@ו�@�S�@�+@�ȴ@�5?@�@�V@��/@ԛ�@�Q�@�9X@���@��
@Ӿw@ӕ�@�S�@���@���@�ff@�$�@Ѳ-@�x�@�G�@�z�@��m@ϥ�@�C�@�ȴ@θR@θR@ΰ!@Ο�@·+@�M�@͙�@�x�@�x�@�x�@���@̋D@�(�@���@˾w@˝�@˅@�C�@��H@ʸR@�^5@��T@�X@ȋD@�K�@�$�@őh@�hs@��@ģ�@�Q�@Ý�@�l�@��
@�(�@�r�@�z�@�bN@��@���@�|�@��y@���@�r�@�b@�1@�b@�(�@�9X@�(�@��m@��@���@��@���@�"�@�@�X@��P@��@��@��@�ȴ@��+@�{@���@�p�@�X@��/@�z�@�r�@�Q�@���@�ȴ@�=q@��7@���@��j@�I�@�b@��@�ƨ@��F@���@�;d@���@���@�~�@�V@��@��@���@�hs@��j@�A�@�  @���@���@�t�@�@�n�@�{@���@�x�@�O�@��9@�b@��w@���@�dZ@�33@��@��!@���@��\@�M�@�@�hs@��j@��D@�r�@�j@� �@�1@�b@��@��@��@��
@�t�@�dZ@�;d@��y@��\@�^5@��@���@��7@�X@�G�@�/@�V@���@��@�Q�@��@��
@�ƨ@��w@��F@��@���@���@��@�o@���@�ff@�$�@��@�hs@�?}@��@�Z@�I�@�1@�ƨ@��F@���@�\)@�"�@�"�@�o@�
=@��@�v�@�^5@��@�K�@�|�@�hs@�r�@{"�@s33@j�!@[@Q�#@G�@?�@9G�@1��@-�-@(r�@#t�@E�@�@b@t�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�JA���Aڝ�Aڟ�A�l�A�5?A�E�A�I�A�5?A�(�A�JA�JA�oA���A��A��`A��#AٶFA�~�A�~�AفAفA�~�AفAه+AًDAى7AًDAًDAمAمAًDAٍPAًDAًDAُ\Aٗ�Aٝ�Aٙ�Aٟ�A־wA�`BA�p�A��A�G�A�n�A�|�A�O�A�A�$�A��mA̲-A�p�A�VA�9XA��TA˴9A�n�A��
AɃA�  AȾwAȏ\A�\)Aƕ�A�ZA�ZA���A�?}A�ffA���A�bNA�z�A�ȴA�(�A�oA��A��A��`A�ƨA��\A�A�(�A��FA��
A�XA��A�K�A�9XA�t�A��A��hA�$�A���A�E�A���A���A�K�A�\)A�M�A�`BA�1'A�A�{A��DA��A���A���A��!A�VA��A���A|�Ay��Ax{Aw�As�mAq+An��AmVAk��Aj�DAj  Ai7LAg7LAe��Ac�PA\ffAY%AX-AV^5AR�/AOhsAOVANjAM�;AM�7AL�`AJ�`AE�;AD-AC+AB-A@v�A>I�A;
=A7G�A3��A3S�A1l�A0A-�;A-�A,ZA+�7A(n�A'�PA&�yA&$�A$�+A"M�A ��A Q�A��A�wAhsA/A�DA?}AjA��AM�AȴA�uA~�A^5AffA`BA��A�Ap�A��AE�AA��A��A��AVA��AC�Az�A��A �A��A
�A��A�A`BA(�A\)A��A��AI�A�^A;dAoA �9A z�A ^5A M�A 1@�x�@�V@�G�@�t�@�&�@�r�@�bN@�A�@�@�\@�n�@�E�@�v�@��@��@��
@�  @�|�@�$�@�-@�h@�&�@���@��m@�R@�V@���@�V@��@�+@�hs@� �@�Q�@�P@�@�+@�"�@�  @�@� �@�r�@�bN@�ƨ@�t�@�o@�!@�ff@�7L@��@�@�"�@�ff@�$�@�/@�j@��
@��@ް!@�{@ݡ�@��@܋D@��@�dZ@�"�@ڟ�@��@�J@��@١�@���@�1'@ו�@�S�@�+@�ȴ@�5?@�@�V@��/@ԛ�@�Q�@�9X@���@��
@Ӿw@ӕ�@�S�@���@���@�ff@�$�@Ѳ-@�x�@�G�@�z�@��m@ϥ�@�C�@�ȴ@θR@θR@ΰ!@Ο�@·+@�M�@͙�@�x�@�x�@�x�@���@̋D@�(�@���@˾w@˝�@˅@�C�@��H@ʸR@�^5@��T@�X@ȋD@�K�@�$�@őh@�hs@��@ģ�@�Q�@Ý�@�l�@��
@�(�@�r�@�z�@�bN@��@���@�|�@��y@���@�r�@�b@�1@�b@�(�@�9X@�(�@��m@��@���@��@���@�"�@�@�X@��P@��@��@��@�ȴ@��+@�{@���@�p�@�X@��/@�z�@�r�@�Q�@���@�ȴ@�=q@��7@���@��j@�I�@�b@��@�ƨ@��F@���@�;d@���@���@�~�@�V@��@��@���@�hs@��j@�A�@�  @���@���@�t�@�@�n�@�{@���@�x�@�O�@��9@�b@��w@���@�dZ@�33@��@��!@���@��\@�M�@�@�hs@��j@��D@�r�@�j@� �@�1@�b@��@��@��@��
@�t�@�dZ@�;d@��y@��\@�^5@��@���@��7@�X@�G�@�/@�V@���@��@�Q�@��@��
@�ƨ@��w@��F@��@���@���@��@�o@���@�ff@�$�@��@�hs@�?}@��@�Z@�I�@�1@�ƨ@��F@���@�\)@�"�@�"�@�o@�
=@��@�v�@�^5G�O�@�K�@�|�@�hs@�r�@{"�@s33@j�!@[@Q�#@G�@?�@9G�@1��@-�-@(r�@#t�@E�@�@b@t�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B%BBB
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�sB
�yB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
^5B
cTB
�DB
�dB
�'B
��B
��B
�VB
�PB
�VB
�hB
��B
�B
�jB
��B
�#B
�ZB
�B	7BhB�B"�B%�BT�Bz�B��B��B�ZB�B��BhB�B+B/B$�B�BVB%B��B�
BŢB�B��B�1B�Bp�B\)BK�B&�BVB1BB
��B
��B
��B
�B
�B
�#B
�qB
�?B
�'B
�B
��B
��B
�oB
�+B
o�B
cTB
ZB
R�B
E�B
-B
�B
hB
PB	��B	�TB	��B	ƨB	�qB	�RB	�9B	�B	��B	�uB	�B	\)B	L�B	G�B	@�B	6FB	,B	+B	(�B	&�B	$�B	�B	�B	%B	B��B��B�B�B�HB�B��B��B��BɺBȴBǮBƨBĜBĜBĜBĜBÖBŢBǮBɺB��B��B��B��B��B��B��B��B�B�#B�NB�TB�TB�NB�B��B��B��B	B	B		7B	JB	PB	PB	JB	bB	�B	�B	�B	!�B	"�B	#�B	&�B	+B	.B	0!B	5?B	8RB	:^B	<jB	>wB	B�B	F�B	F�B	H�B	I�B	I�B	I�B	I�B	L�B	P�B	Q�B	O�B	K�B	J�B	I�B	I�B	G�B	G�B	I�B	O�B	R�B	Q�B	M�B	M�B	P�B	R�B	S�B	T�B	S�B	T�B	VB	[#B	`BB	aHB	bNB	o�B	{�B	z�B	x�B	{�B	�B	� B	� B	� B	�+B	�PB	��B	�!B	�3B	�9B	�9B	�9B	�?B	�?B	�?B	�FB	�9B	�3B	�3B	�3B	�9B	�?B	�FB	�LB	�RB	�RB	�RB	�FB	�?B	�9B	�9B	�?B	�9B	�3B	�-B	�9B	�LB	�LB	�jB	�wB	�wB	�wB	�}B	��B	��B	B	B	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	�B	�B	�/B	�B	�B	�B	�B	�)B	�/B	�;B	�HB	�BB	�BB	�HB	�NB	�TB	�ZB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�fB	�`B	�ZB	�`B	�fB	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
JB
hB
�B
!�B
'�B
.B
2-B
49B
>wB
C�B
K�B
Q�B
YB
aHB
dZB
iyB
l�B
q�B
t�B
v�B
w�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
BB�B�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�{B
�uB
�nB
�nB
�\B
�PB
�WB
�]B
�gB
�tB
�tB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
^B
c6B
� B
�CB
�B
��B
�kB
�2B
�+B
�3B
�FB
��B
��B
�FB
��B
��B
�5B
�B	BBB|B"�B%�BT�Bz�B�[BαB�0B�bB��B?BwB*�B.�B$�B~B*B�B��B��B�vB��B�sB�B��BpuB[�BK�B&�B(BB�B
��B
��B
��B
�yB
�TB
��B
�GB
�B
��B
��B
��B
�{B
�BB
��B
osB
c'B
Y�B
R�B
EyB
,�B
wB
<B
'B	��B	�+B	��B	�~B	�HB	�-B	�B	��B	��B	�NB	��B	\B	L�B	G�B	@`B	6 B	+�B	*�B	(�B	&�B	$�B	�B	`B	 B	 �B��B��B�B�cB�'B��B��BϻB̧BɗBȐBǇBƂB�yB�wB�yB�yB�rB�|BǈBɖBˡBͭBͮBΰBβBϸB��B��B��B��B�&B�,B�,B�'B�oB��B��B��B	 �B	�B		B	B	'B	'B	B	8B	hB	tB	�B	!�B	"�B	#�B	&�B	*�B	-�B	/�B	5B	8&B	:3B	<>B	>KB	BbB	F{B	FxB	H�B	I�B	I�B	I�B	I�B	L�B	P�B	Q�B	O�B	K�B	J�B	I�B	I�B	G�B	G�B	I�B	O�B	R�B	Q�B	M�B	M�B	P�B	R�B	S�B	T�B	S�B	T�B	U�B	Z�B	`B	aB	bB	ooB	{�B	z�B	x�B	{�B	��B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�"B	� B	�B	�B	�B	�B	�
B	�B	� B	��B	�B	�B	�B	�6B	�CB	�DB	�DB	�IB	�QB	�OB	�^B	�]B	�cB	�hB	�mB	�|B	ɇB	ˑB	΢B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�B	�B	�!B	�B	�$B	�+B	�+B	�JB	�PB	�IB	�HB	�HB	�NB	�IB	�VB	�[B	�cB	�oB	�uB	�hB	�[B	�UB	�IB	�>B	�9B	�3B	�+B	�$B	�,B	�1B	�IB	�bB	�B	�B	�B	�B	�yB	�{B	�{B	�xB	�tB	�uB	�xB	�B	��B	��B	��B	��B	�yB	�oB	�xB	�B	�lB	�UB	�VB	�PB	�gB	�mB	�mB	�sB	�wB	�kB	�kB	�nB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B
�B
	B
B
B
B
G�O�B
1B
RB
!�B
'�B
-�B
1�B
4B
>?B
C]B
K�B
Q�B
X�B
a
B
d!B
i=B
lTB
qoB
t�B
v�B
w�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451312016080714513120160807145131  AO  ARCAADJP                                                                    20160629092330    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160629092330  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160629092330  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145131  IP                  G�O�G�O�G�O�                