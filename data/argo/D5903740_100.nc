CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-12-15T06:02:00Z AOML 3.0 creation; 2016-06-01T00:08:22Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141215060200  20160531170822  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               dA   AO  4055_7112_100                   2C  D   APEX                            5374                            041511                          846 @�*�|ƀ
1   @�*�$��@9���E��d7�
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    dA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D� D�FfD���D���D�  D�L�D�vfD���D�  D�@ D�vfD�� D�	�D�6fDڜ�D��3D� D�C3D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���Az�A$z�ADz�Adz�A�=qA�=qA�p�A�=qA�=qA�=qA�=qA�=qB�B�RB�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B�B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXaHCZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D��D�O\D���D���D�(�D�U�D�\D���D��D�H�D�\D���D��D�?\Dڥ�D��)D��D�L)D�\D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��mA��;A��A�ĜA�|�A�A�ƨA��#A��jA���A��A�p�A�jA�n�A�?}A�7LA�5?A�7LA�33A�-A�$�A�  A��;A�ĜA�l�A��A���A���A��FA��FA��wA�A�ĜA�ƨA�ƨA�ĜA��jA��A�n�A��A�;dA�"�A��uA��-A�
=A�VA�ƨA���A�;dA�VA��-A��A���A���A��^A��
A�\)A�?}A�dZA���A�`BA�ffA���A�p�A���A���A�+A��wA��yA�5?A�^5A��A���A��uA��A��^A�XA��wA�O�A�ȴA�\)A���A��A���A�bAoA|9XA{;dAz�\Ay�Ay��Ax��Aw%Av��At�AoC�Am
=AkƨAj�!Ah��Agl�Ae�TAd��Ac�Ac\)Ab�9A`�jA\�A\��A[�PA[p�A[S�AZ�/AZVAWoAS�AQS�APZAO��AOK�ANbNAM/AKp�AI
=AFQ�AE33AD��AD1AC�7ABȴAA��AAVA@�A@��A@�A@ �A>$�A;��A;"�A:�A8��A7�-A6��A6�uA6v�A6=qA5�PA4�A4�A4�uA3�A29XA1��A1XA0�yA0�DA/C�A.=qA-�7A,ĜA,1'A+�7A+S�A+�A*��A)�A({A'/A&��A&r�A&I�A%��A$�RA#l�A#A"�uA"(�A!�A!�FA!��A�#A1'A��A��AhsAG�A��A�AJA&�A$�A��A�AM�A�A��A�DAVA1A�mA�+A`BA�yAVA��AĜAȴA�A��AdZA-A��A
��A
�uA
^5A
Q�A
�A	dZA5?A"�A �A�DAoA  AS�A ��A �A 5?@�S�@���@��/@�t�@�=q@�?}@��@���@��\@�hs@�?}@��@��@�F@�l�@�o@��@�~�@��@�K�@�E�@噚@�(�@��@��@�%@�Z@�-@�33@�^5@�J@��@��@��#@ٺ^@١�@�x�@�O�@�/@� �@�E�@�z�@�@�hs@���@��/@ʟ�@�hs@�z�@���@���@őh@���@��m@§�@���@���@��j@�1'@��
@��y@���@���@�A�@��R@��@�@�7L@��!@�V@�j@�  @�t�@�
=@��@�ff@��^@���@��@��P@�C�@�~�@�J@���@�&�@���@�b@�C�@��R@�V@���@�dZ@���@�@�X@��@��9@�Z@�b@���@�S�@�
=@���@�^5@�=q@�@�@��^@�x�@�V@��@�j@�1'@��
@�@�-@��#@�7L@��D@���@�~�@��7@��/@�j@�1@��@��@�ȴ@��+@�E�@�J@�@��7@��@��@��m@��P@�l�@��@�~�@�M�@��@���@�hs@�/@���@���@�z�@���@���@���@�r�@�1'@�|�@���@�~�@�^5@�=q@��@��@���@���@�X@���@��D@�Z@�Q�@�Z@�Z@�Q�@�9X@��m@���@�\)@��@�^5@��@���@��^@���@�hs@�?}@�&�@���@��@��@�Q�@�1'@��@�b@�1@�1@�b@�b@���@��P@�+@�E�@�@��T@���@���@���@���@���@���@���@���@���@��7@�p�@�/@�%@���@���@���@��`@���@��9@��@�A�@�@|�@+@
=@~�y@~�y@~�y@~��@~{@}�T@}��@}�@|�@{�@{"�@z�H@z��@z��@z�@y�7@yG�@y�@x�`@x��@xĜ@x�9@x�u@xbN@x �@x �@w��@w|�@v�R@v5?@u?}@t�/@s�m@q��@h �@_
=@Zn�@P �@F��@BM�@<I�@65?@/�w@*n�@&$�@ �9@9X@ �@��@7L@v�@
~�@��@ �`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A��mA��;A��A�ĜA�|�A�A�ƨA��#A��jA���A��A�p�A�jA�n�A�?}A�7LA�5?A�7LA�33A�-A�$�A�  A��;A�ĜA�l�A��A���A���A��FA��FA��wA�A�ĜA�ƨA�ƨA�ĜA��jA��A�n�A��A�;dA�"�A��uA��-A�
=A�VA�ƨA���A�;dA�VA��-A��A���A���A��^A��
A�\)A�?}A�dZA���A�`BA�ffA���A�p�A���A���A�+A��wA��yA�5?A�^5A��A���A��uA��A��^A�XA��wA�O�A�ȴA�\)A���A��A���A�bAoA|9XA{;dAz�\Ay�Ay��Ax��Aw%Av��At�AoC�Am
=AkƨAj�!Ah��Agl�Ae�TAd��Ac�Ac\)Ab�9A`�jA\�A\��A[�PA[p�A[S�AZ�/AZVAWoAS�AQS�APZAO��AOK�ANbNAM/AKp�AI
=AFQ�AE33AD��AD1AC�7ABȴAA��AAVA@�A@��A@�A@ �A>$�A;��A;"�A:�A8��A7�-A6��A6�uA6v�A6=qA5�PA4�A4�A4�uA3�A29XA1��A1XA0�yA0�DA/C�A.=qA-�7A,ĜA,1'A+�7A+S�A+�A*��A)�A({A'/A&��A&r�A&I�A%��A$�RA#l�A#A"�uA"(�A!�A!�FA!��A�#A1'A��A��AhsAG�A��A�AJA&�A$�A��A�AM�A�A��A�DAVA1A�mA�+A`BA�yAVA��AĜAȴA�A��AdZA-A��A
��A
�uA
^5A
Q�A
�A	dZA5?A"�A �A�DAoA  AS�A ��A �A 5?@�S�@���@��/@�t�@�=q@�?}@��@���@��\@�hs@�?}@��@��@�F@�l�@�o@��@�~�@��@�K�@�E�@噚@�(�@��@��@�%@�Z@�-@�33@�^5@�J@��@��@��#@ٺ^@١�@�x�@�O�@�/@� �@�E�@�z�@�@�hs@���@��/@ʟ�@�hs@�z�@���@���@őh@���@��m@§�@���@���@��j@�1'@��
@��y@���@���@�A�@��R@��@�@�7L@��!@�V@�j@�  @�t�@�
=@��@�ff@��^@���@��@��P@�C�@�~�@�J@���@�&�@���@�b@�C�@��R@�V@���@�dZ@���@�@�X@��@��9@�Z@�b@���@�S�@�
=@���@�^5@�=q@�@�@��^@�x�@�V@��@�j@�1'@��
@�@�-@��#@�7L@��D@���@�~�@��7@��/@�j@�1@��@��@�ȴ@��+@�E�@�J@�@��7@��@��@��m@��P@�l�@��@�~�@�M�@��@���@�hs@�/@���@���@�z�@���@���@���@�r�@�1'@�|�@���@�~�@�^5@�=q@��@��@���@���@�X@���@��D@�Z@�Q�@�Z@�Z@�Q�@�9X@��m@���@�\)@��@�^5@��@���@��^@���@�hs@�?}@�&�@���@��@��@�Q�@�1'@��@�b@�1@�1@�b@�b@���@��P@�+@�E�@�@��T@���@���@���@���@���@���@���@���@���@��7@�p�@�/@�%@���@���@���@��`@���@��9@��@�A�@�@|�@+@
=@~�y@~�y@~�y@~��@~{@}�T@}��@}�@|�@{�@{"�@z�H@z��@z��@z�@y�7@yG�@y�@x�`@x��@xĜ@x�9@x�u@xbN@x �@x �@w��@w|�@v�R@v5?@u?}@t�/G�O�@q��@h �@_
=@Zn�@P �@F��@BM�@<I�@65?@/�w@*n�@&$�@ �9@9X@ �@��@7L@v�@
~�@��@ �`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBl�Bl�Bl�Bk�BjBjBiyBffB`BB]/B^5B\)B[#BYBYBXBXBVBT�BT�BT�BT�BS�BR�BN�BK�BG�B?}B7LB,B33B8RB8RB:^B;dB<jB<jB<jB<jB;dB8RB1'B'�BhB��B�mB��B�\BS�B1'B1BB�B��B�\B�BiyBYBI�B=qBPB��B�ZBŢB�B�DBu�BjBgmB`BBYBI�B<jB.B'�B"�B�BhB
��B
��B
�LB
�B
��B
}�B
r�B
l�B
aHB
VB
L�B
7LB
-B
&�B
"�B
�B
�B
JB
B	�B	��B	��B	�LB	�3B	�B	��B	��B	��B	�bB	�JB	�%B	v�B	aHB	^5B	YB	YB	XB	T�B	N�B	<jB	(�B	�B	�B	uB	bB	JB	B��B�B�B�yB�sB�fB�`B�TB�HB�BB�BB�;B�5B�#B�B��B��B��BɺBǮBŢBÖBÖB��B��B�}B�wB�jB�XB�dB�dB�^B�XB�LB�9B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B�{B�bB�\B�VB�PB�DB�=B�7B�%B� B{�Bz�Bz�By�Bx�Bw�Bu�Bs�Bp�Bm�Bk�BiyBe`BbNBaHBaHB`BB^5B\)BXBT�BR�BP�BM�BH�BD�BC�BA�B?}B<jB:^B8RB8RB7LB7LB5?B2-B/B,B'�B#�B!�B�B�B�B�B�B�B�B�B�B{BuBoBoBbBPBDBDBDBDBDBDB
=B	7B1B+B+B%BBBBBBBBBBBBBBBBBBBBBBBBBB%B%B1B	7B	7B	7B
=BJBPBVBVB\B\BbBbBhBuB�B�B�B�B�B �B"�B#�B$�B%�B%�B&�B'�B+B-B.B.B0!B1'B2-B33B49B5?B8RB9XB9XB=qBC�BE�BH�BK�BL�BM�BN�BO�BQ�BR�BT�BVBW
BXBXBZBZB[#B\)B^5B_;B`BBaHBdZBhsBiyBl�Bo�Bq�Bx�B}�B�B�B�%B�7B�JB�VB�\B�hB�oB�{B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�9B�?B�LB�jB��B��B�B�B�BB�sB�B�B�B�B�B�B�B��B��B��B��B	  B��B��B	  B	B	%B	
=B	PB	\B	�B	�B	�B	 �B	"�B	$�B	&�B	'�B	)�B	.B	0!B	2-B	49B	5?B	7LB	9XB	:^B	;dB	<jB	@�B	B�B	C�B	J�B	O�B	Q�B	W
B	W
B	XB	YB	YB	ZB	ZB	ZB	ZB	[#B	\)B	bNB	iyB	l�B	l�B	m�B	p�B	r�B	s�B	v�B	x�B	{�B	|�B	~�B	~�B	� B	� B	� B	�B	�B	�B	�%B	�1B	�PB	�\B	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	�!B	ɺB	�BB	�B
B
�B
�B
)�B
2-B
<jB
C�B
H�B
P�B
VB
[#B
`BB
dZB
gmB
l�B
s�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bl}BlBlBkwBjqBjrBilBfXB`;B]#B^'B\B[BYBY	BXBXBU�BT�BT�BT�BT�BS�BR�BN�BK�BG�B?pB7>B+�B3$B8DB8FB:NB;WB<^B<\B<\B<\B;WB8DB1B'�BXB��B�]B��B�IBS�B1BB�wB��B��B�CB��Bi]BX�BI�B=WB4B��B�>BŅB��B�*Bu�BjfBgUB`)BX�BI�B<PB-�B'�B"�B�BLB
��B
��B
�3B
��B
��B
}�B
r�B
luB
a0B
U�B
L�B
76B
,�B
&�B
"�B
�B
�B
6B
B	�B	ͿB	�rB	�8B	�"B	��B	��B	��B	�lB	�QB	�7B	�B	v�B	a8B	^$B	YB	YB	X B	T�B	N�B	<ZB	(�B	�B	B	hB	UB	;B	B��B�B�zB�lB�gB�YB�QB�GB�;B�4B�5B�0B�)B�B��B��B��B��BɯBǠBŕBÉBÈB�~B�wB�pB�jB�]B�KB�YB�YB�QB�JB�BB�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B�{B�rB�XB�TB�JB�FB�;B�2B�.B�B�B{�Bz�Bz�By�Bx�Bw�Bu�Bs�Bp�Bm�BkzBioBeXBbDBa@BaAB`8B^-B\ BXBT�BR�BP�BM�BH�BD�BC�BAB?uB<aB:UB8/B8HB7DB7)B58B2%B/B+�B'�B#�B!�B�B�B�B�B�BxB�BjB[BXBnBLBNBAB.B"B#B"B<B<B<B
B	BB	BBB�B�B�B�B
B�B�B�B�B�B�B�B�B�BB�B�B �B �B �B�B�B �B�B�BBBB	B	/B	B
B)B+B2B4BRBQB<B=BaBiB�B�B�B�B�B �B"�B#�B$�B%�B%�B&�B'�B*�B-B.B.B0B1B2 B3'B4+B53B8CB9MB9MB=eBC�BE�BH�BK�BL�BM�BN�BO�BQ�BR�BT�BU�BV�BXBXBZBZB[B\B^'B_,B`4Ba9BdJBhdBijBl{Bo�Bq�Bx�B}�B��B�B�B�'B�9B�FB�JB�WB�ZB�hB�oB�zB��B��B��B��B��B��B��B��B�B�B�!B�&B�,B�8B�XB˴B��B��B�B�-B�^B�iB�pB�uB�zB�B��B�B��B��B��B��B��B��B��B��B	�B	B	
%B	9B	EB	{B	�B	�B	 �B	"�B	$�B	&�B	'�B	)�B	-�B	0	B	2B	4"B	5'B	73B	9?B	:EB	;MB	<RB	@kB	BwB	C|B	J�B	O�B	Q�B	V�B	V�B	W�B	X�B	Y B	ZB	ZB	ZB	ZB	[
B	\B	b3B	i\B	lqB	lqB	muB	p�B	r�B	s�B	v�B	x�B	{�B	|�B	~�B	~�B	�B	�B	�B	��B	��B	�B	�	B	�B	�4B	�?B	�KB	�OB	�TB	�XB	�eB	�pB	�xB	�|B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	ɜB	�&B	�gB
�B
nB
�B
)�B
2B
<MB
CyB
H�B
P�B
U�B
[B
`$B
d8B
gNB
llB
s�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.28 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708222016053117082220160531170822  AO  ARCAADJP                                                                    20141215060200    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141215060200  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141215060200  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170822  IP                  G�O�G�O�G�O�                