CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:32Z AOML 3.0 creation; 2016-06-01T00:08:14Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230832  20160531170814  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               5A   AO  4055_7112_053                   2C  D   APEX                            5374                            041511                          846 @ֲ�K�
1   @ֲ��
`@9�r� Ĝ�c��
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    5A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@ffBG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dyl�D�fD�<�D��3D�� D�fD�9�D��3D��3D�fD�FfD�� D�� D�3D�9�Dڃ3D���D�fD�6fD�s3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @E�@���@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�
>B�B	�B�B�B!�B)�B1�B9�BA�BH�RBQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\.C^G�C`G�CbG�CdaHCfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy~�D�\D�E�D��)D���D�\D�B�D��)D��)D�\D�O\D���D���D�)D�B�Dڌ)D���D�\D�?\D�|)D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�p�A�n�A�r�A�A�A�E�A�?}A��`A��A���AѴ9AэPA�C�A���A�|�A�A�A�
=A���Aϲ-A�JA� �A�"�A�5?Aß�A��A�\)A��A�;dA�r�A���A���A�;dA��A��A���A��A�?}A�JA�A�hsA��uA���A�jA�n�A�~�A�A�A�1A���A��wA�VA���A��A�A�A��7A��;A�`BA��yA��wA���A� �A�`BA��#A�C�A��mA���A�
=A��DA��A���A��uA�1A��7A�7LA��A��\A�$�A��7A�hsA� �A�A�l�A��A�r�A�S�A�A�/A��jA��`A�{A��A�Q�A���A��A�|�A��A��jA���A�5?A�\)A�{A�ZA�I�A��^A���A�;dA�/A}\)A{?}AyVAx  Aq��AnĜAm"�AljAk
=Ah��Ab��Aa%A`n�A]��A]7LA[��AXbNAW`BAVz�AU��AS�AO+AL(�AKhsAJ��AH�jAGS�AF�+AE��AD�AB�ABbAA%A@z�A?��A>�A>9XA=�PA<jA;�A:�DA:1'A8�yA7�PA5�A3�-A2M�A1l�A0ZA/7LA.�uA-�A,��A+��A*ĜA*JA)��A)dZA)33A(��A'ƨA'x�A';dA&�!A&(�A%"�A$$�A#��A#VA"M�A!�PA jA�;A�A9XA�;A��AG�A&�AoA�yAQ�AXA�\A=qA��AhsAoAZAS�A�DAA�!A��A��A"�A�A?}AVA�A(�A�PAK�A33A
�yA
~�A	�A~�Ax�A+AE�A��A�HAVA�
AdZA��A;d@�@��@�|�@�$�@�hs@�%@�  @�v�@�G�@���@�w@�@�9@�j@�l�@�E�@���@��@���@�/@�F@�v�@�X@�9@�Q�@㝲@��y@�7@�j@�1@�K�@�%@�S�@�G�@���@�33@�v�@�G�@ӕ�@�C�@ҏ\@Ѻ^@У�@ύP@���@�{@��@�X@�r�@��m@���@ɺ^@�`B@��`@ǅ@Ų-@Ĭ@�A�@��
@Ý�@�C�@�@���@�/@�Ĝ@���@�(�@�dZ@��@�=q@�G�@���@�;d@�$�@��u@�K�@�V@���@��@���@�C�@��@�E�@��-@�G�@��@�9X@��@���@���@��@��9@���@�\)@���@��!@�v�@��@�x�@��@��D@� �@���@��;@�C�@�^5@�?}@�Q�@���@��P@���@�ff@��T@�G�@���@� �@��w@�l�@���@�=q@���@�G�@��/@���@�j@�(�@��@�
=@��!@�-@��#@�p�@���@��`@��9@� �@�|�@��R@�E�@���@�X@�&�@�V@���@�Ĝ@� �@��F@�|�@���@�@��^@��7@�x�@�O�@�%@��j@��@��@��P@�\)@�C�@�33@�
=@��+@�M�@���@�@���@���@�x�@��@���@��@��D@��@��w@�t�@�C�@�+@�o@��@�ȴ@��!@���@�ff@�5?@��T@���@�O�@��@��j@��D@�I�@�b@���@���@�S�@�;d@��@���@���@��y@���@��R@���@���@�v�@�ff@�E�@�5?@�{@���@�x�@�`B@�G�@�&�@���@��`@���@��9@���@�r�@� �@�@\)@�@~V@}�-@}�@|��@|z�@{��@{33@z��@y�@y�7@yX@y7L@y&�@x��@x��@x�9@xQ�@x �@x  @w�P@w+@v�y@v$�@up�@u?}@uV@t�j@t(�@s�m@s�m@s��@s"�@r~�@r�@q��@q��@qX@pĜ@o|�@lI�@c"�@^V@W|�@Qhs@H1'@B�@<��@7
=@0�`@*��@%@ ��@Z@A�@/@bN@�@A�@�/@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�p�A�n�A�r�A�A�A�E�A�?}A��`A��A���AѴ9AэPA�C�A���A�|�A�A�A�
=A���Aϲ-A�JA� �A�"�A�5?Aß�A��A�\)A��A�;dA�r�A���A���A�;dA��A��A���A��A�?}A�JA�A�hsA��uA���A�jA�n�A�~�A�A�A�1A���A��wA�VA���A��A�A�A��7A��;A�`BA��yA��wA���A� �A�`BA��#A�C�A��mA���A�
=A��DA��A���A��uA�1A��7A�7LA��A��\A�$�A��7A�hsA� �A�A�l�A��A�r�A�S�A�A�/A��jA��`A�{A��A�Q�A���A��A�|�A��A��jA���A�5?A�\)A�{A�ZA�I�A��^A���A�;dA�/A}\)A{?}AyVAx  Aq��AnĜAm"�AljAk
=Ah��Ab��Aa%A`n�A]��A]7LA[��AXbNAW`BAVz�AU��AS�AO+AL(�AKhsAJ��AH�jAGS�AF�+AE��AD�AB�ABbAA%A@z�A?��A>�A>9XA=�PA<jA;�A:�DA:1'A8�yA7�PA5�A3�-A2M�A1l�A0ZA/7LA.�uA-�A,��A+��A*ĜA*JA)��A)dZA)33A(��A'ƨA'x�A';dA&�!A&(�A%"�A$$�A#��A#VA"M�A!�PA jA�;A�A9XA�;A��AG�A&�AoA�yAQ�AXA�\A=qA��AhsAoAZAS�A�DAA�!A��A��A"�A�A?}AVA�A(�A�PAK�A33A
�yA
~�A	�A~�Ax�A+AE�A��A�HAVA�
AdZA��A;d@�@��@�|�@�$�@�hs@�%@�  @�v�@�G�@���@�w@�@�9@�j@�l�@�E�@���@��@���@�/@�F@�v�@�X@�9@�Q�@㝲@��y@�7@�j@�1@�K�@�%@�S�@�G�@���@�33@�v�@�G�@ӕ�@�C�@ҏ\@Ѻ^@У�@ύP@���@�{@��@�X@�r�@��m@���@ɺ^@�`B@��`@ǅ@Ų-@Ĭ@�A�@��
@Ý�@�C�@�@���@�/@�Ĝ@���@�(�@�dZ@��@�=q@�G�@���@�;d@�$�@��u@�K�@�V@���@��@���@�C�@��@�E�@��-@�G�@��@�9X@��@���@���@��@��9@���@�\)@���@��!@�v�@��@�x�@��@��D@� �@���@��;@�C�@�^5@�?}@�Q�@���@��P@���@�ff@��T@�G�@���@� �@��w@�l�@���@�=q@���@�G�@��/@���@�j@�(�@��@�
=@��!@�-@��#@�p�@���@��`@��9@� �@�|�@��R@�E�@���@�X@�&�@�V@���@�Ĝ@� �@��F@�|�@���@�@��^@��7@�x�@�O�@�%@��j@��@��@��P@�\)@�C�@�33@�
=@��+@�M�@���@�@���@���@�x�@��@���@��@��D@��@��w@�t�@�C�@�+@�o@��@�ȴ@��!@���@�ff@�5?@��T@���@�O�@��@��j@��D@�I�@�b@���@���@�S�@�;d@��@���@���@��y@���@��R@���@���@�v�@�ff@�E�@�5?@�{@���@�x�@�`B@�G�@�&�@���@��`@���@��9@���@�r�@� �@�@\)@�@~V@}�-@}�@|��@|z�@{��@{33@z��@y�@y�7@yX@y7L@y&�@x��@x��@x�9@xQ�@x �@x  @w�P@w+@v�y@v$�@up�@u?}@uV@t�j@t(�@s�m@s�m@s��@s"�@r~�@r�@q��@q��@qX@pĜ@o|�@lI�@c"�@^V@W|�@Qhs@H1'@B�@<��@7
=@0�`@*��@%@ ��@Z@A�@/@bN@�@A�@�/@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBffBffBffBe`BdZBdZBdZBdZBdZBe`Be`BdZBdZBbNB_;B^5B\)BYBVBL�B.BB�B�5B��B�qB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�PB�%B~�B|�B{�Bz�By�Bt�Bq�Bn�Bl�BhsBdZBdZBcTBdZBdZBaHB^5BZBVBQ�BL�BD�B=qB5?B2-B.B#�B�B�BhBDBB��B��B�B�mB�HB�
B��BȴB�}B�B��Bo�BT�B$�B��BĜB��Bz�B^5BE�B49B)�B�BB
��B
�/B
��B
ffB
F�B
33B
�B
	7B	��B	�yB	�wB	��B	��B	��B	�=B	u�B	P�B	F�B	@�B	0!B	+B	 �B	VB	1B	B��B�B�#B��B��BɺBB�wB�^B�LB�9B�'B�-B�3B�?B�FB�FB�FB�?B�9B�-B�!B�B�B��B��B��B�uB�\B�JB�+B�B� B}�Bz�By�By�By�Bx�Bx�Bv�Bv�Bv�Bu�Bt�Bs�Br�Bo�Bn�Bm�Bk�BiyBgmBe`BdZBcTBcTBbNBbNBbNBaHB`BB_;B]/B\)B[#BYBXBW
BT�BQ�BN�BK�BH�BC�B@�B=qB:^B7LB5?B2-B0!B/B.B.B-B+B(�B'�B&�B%�B%�B#�B"�B �B�B�B�B�B�B{BuBoBhBbB\BVBVBVBPBJBDB
=B
=B	7B1B+B%BBBBBBBBBBBB  BBBBBB  B  BBBBBBB%B	7B
=BJBhBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B$�B%�B)�B,B1'B5?B8RB:^B<jB=qB@�BA�BB�BC�BE�BF�BF�BH�BL�BM�BM�BP�BS�BVBXBYBZB[#B]/B^5B_;BbNBe`Be`BdZBffBiyBn�Bt�Bv�Bv�Bz�B}�B� B�B�%B�=B�DB�VB�hB�{B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�9B�?B�XB�qBBŢBȴB��B��B��B��B��B��B�B�
B�/B�TB�`B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	+B	DB	PB	VB	\B	\B	oB	�B	�B	�B	�B	�B	"�B	$�B	%�B	%�B	'�B	)�B	+B	,B	.B	/B	2-B	5?B	7LB	;dB	=qB	?}B	A�B	C�B	D�B	G�B	K�B	M�B	O�B	Q�B	Q�B	R�B	T�B	VB	W
B	W
B	ZB	ZB	\)B	\)B	]/B	aHB	cTB	dZB	dZB	e`B	gmB	hsB	iyB	jB	k�B	m�B	p�B	s�B	u�B	v�B	z�B	|�B	}�B	�B	�B	�B	�+B	�7B	�VB	�bB	�hB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�9B	�RB	��B	�/B	�B	��B
	7B
�B
�B
(�B
2-B
;dB
E�B
M�B
S�B
YB
^5B
cTB
gmB
l�B
r�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bf[Bf[BfXBeUBdOBdQBdQBdPBdRBeXBeWBdRBdRBbEB_1B^,B\ BYBU�BL�B.B�B�B�$B̼B�bB�,B�B��B��B��B��B��B��B��B��B��B��B�xB�rB�XB�?B�<B�B~�B|�B{�Bz�By�Bt�Bq�Bn�BlwBh\BdDBdDBc>BdABdDBa2B^BZBU�BQ�BL�BD�B=XB5'B2B-�B#�B�BpBNB.B�B��B��B�B�UB�/B��B̵BȝB�bB��B��Bo�BT�B$�B��BĂB��Bz�B^BE�B4B)�ByB�B
��B
�B
��B
fQB
F�B
3B
�B
	&B	��B	�cB	�cB	��B	��B	�oB	�+B	u�B	P�B	F�B	@sB	0B	*�B	 �B	IB	$B	B��B�~B�B��B��BɯBB�nB�SB�AB�0B�B�!B�)B�4B�:B�=B�9B�4B�-B�!B�B�
B��B��B��B��B�kB�SB�AB�"B�B�B}�Bz�By�By�By�Bx�Bx�Bv�Bv�Bv�Bu�Bt�Bs�Br�Bo�Bn�Bm�BkBiqBgcBeUBdQBcMBcLBbDBbDBbCBaCB`;B_2B]&B\!B[BYBXBW BT�BQ�BN�BK�BH�BC�B@{B=MB:VB7DB56B2B/�B.�B.
B.B,�B*�B(�B'�B&�B%�B%�B#�B"�B �B�B�B�BjB_BsBPBMBEB?B9B5B1BPB.B'B<B
B
8B	/BB#BB�B�BB�B�B�BB�B�B�B �B��B �B �B �B �B �B��B��B�B�B�BB�B�B B	-B
B%BFBhBYB}BcByBcB�B{B�B�B�B�B�B�B�B�B �B!�B"�B"�B$�B%�B)�B+�B1B52B8GB:SB<[B=cB@wBA~BB�BC�BE�BF�BF�BH�BL�BM�BM�BP�BS�BU�BW�BYBZB[B]B^%B_/Bb=BeQBePBdLBfWBijBn�Bt�Bv�Bv�Bz�B}�B�B�B�B�-B�5B�GB�WB�hB�sB��B��B��B��B��B��B��B��B��B�B�B�&B�'B�,B�DB�\B�{BŎBȠB˲B̸B̻B;BͽB��B��B��B�B�>B�KB�YB�_B�hB�vB�B��B��B��B��B��B��B	 �B		B	B	+B	;B	BB	EB	CB	YB	hB	uB	�B	�B	�B	"�B	$�B	%�B	%�B	'�B	)�B	*�B	+�B	-�B	/B	2B	5'B	73B	;JB	=ZB	?dB	AqB	CzB	D�B	G�B	K�B	M�B	O�B	Q�B	Q�B	R�B	T�B	U�B	V�B	V�B	ZB	ZB	\B	\B	]B	a.B	c:B	d@B	d@B	eGB	gQB	hZB	i]B	jdB	kiB	muB	p�B	s�B	u�B	v�B	z�B	|�B	}�B	��B	��B	��B	�B	�B	�9B	�GB	�LB	�LB	�MB	�RB	�ZB	�bB	�kB	�qB	�xB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�8B	�gB	�B	�hB	��B
	B
cB
�B
(�B
2B
;BB
E�B
M�B
S�B
X�B
^B
c5B
gNB
ljB
r�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.28 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708142016053117081420160531170814  AO  ARCAADJP                                                                    20140721230832    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230832  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230832  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170814  IP                  G�O�G�O�G�O�                