CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:29Z AOML 3.0 creation; 2016-06-01T00:08:13Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230829  20160531170813  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               0A   AO  4055_7112_048                   2C  D   APEX                            5374                            041511                          846 @֥�<��1   @֥����@:z�G��c�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    0A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy�3D�	�D�,�D�s3D��3D��D�@ D�|�D��3D�	�D�L�D�� D�� D�	�D�9�D�i�D�ɚD�  D�6fD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��]@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2.C4.C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DKRDK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtq�Dy�D��D�5�D�|)D��)D�%�D�H�D���D��)D��D�U�D���D���D��D�B�D�r�D�ҐD��D�?\D�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʡ�Aɇ+A��A��/A��A��/Ağ�A�C�A��mAÙ�A�M�A�A��A��A�A�n�A��!A��A���A�/A�ƨA�E�A���A��A�=qA���A�$�A�v�A�A�-A���A��+A��A�;dA�z�A���A���A���A�7LA�+A�9XA�5?A���A�"�A��A��7A�1'A�I�A�&�A�
=A��RA�^5A��mA���A�n�A�A�ȴA��A��A�
=A��^A��7A�ffA�?}A��A��A��;A���A�ZA�&�A��A�~�A�;dA���A��A���A��A�K�A��PA���A���A���A�ZA��;A�p�A���A��A�~�A��/A�oA�"�A��jA�+A�$�A�n�A�jA���A|�yAy�AwS�AtI�Ar�9Aq�mAq"�ApM�Ao��AnȴAm�Aj^5Ag�AdZAcl�Aa��Aa�A`z�A_�mA_�A]��AZ�DAXbNAW�TAU?}AR�uAR(�AQ�TAQXAO��AN�yAN-AM�AK��AKG�AJ��AI�AI�-AH��AG��AG"�AFE�AEhsADQ�AC��ABȴAA��A@A�A>�A>$�A=VA<jA;�PA:I�A9�mA8E�A7ƨA6�A5ƨA4r�A3�7A2jA0��A/�mA.r�A-�;A-XA,�RA*�A)+A'��A& �A$��A$1'A#K�A"ĜA"��A"bA!K�A ȴA VA��A�RA�
A|�A��A��A�!A�A�TAl�A{A��An�A(�A��A�AoA=qA�A1A�-AXA�+A�-A�`A9XA��AS�A��AE�AS�A5?A�wA
�`A
VA	C�A �A;dA�mA+An�A�FAVAffAl�A �9A �@�;d@��@��@�1'@�"�@�G�@�"�@���@�&�@�@�@�
=@��@�@�"�@�@��/@��@���@��@�+@�-@��`@�@���@�9X@�@�;d@��@���@�  @�K�@ޏ\@���@ܛ�@���@ڟ�@��T@ش9@�\)@Ցh@ԣ�@�  @Ұ!@��@��@ΰ!@��@�G�@�dZ@��@���@��@ʰ!@ɉ7@���@ǝ�@���@�Q�@�@���@�&�@��@�bN@� �@��@�C�@��@�n�@�@��@�&�@�I�@�o@�$�@�?}@�bN@��P@�;d@���@��^@���@��+@��j@��@��@�7L@� �@�|�@�K�@��!@��@���@��7@�Ĝ@���@��R@�$�@��@�G�@���@��-@��@�Ĝ@�Q�@��@��P@�M�@���@��R@��#@���@�x�@�/@���@���@�(�@�l�@�;d@�
=@��@��+@��@�p�@�j@��@�l�@�~�@��^@��7@��@���@���@���@���@�~�@�E�@��-@��@�1@��@��@��@�v�@�5?@��T@���@�Ĝ@�r�@�bN@�Q�@�A�@�b@��@��@��R@�5?@���@���@��@�O�@��@���@�Z@��@��@�l�@�;d@�o@��@��!@�n�@�$�@���@�x�@�7L@�%@��@��j@���@��D@�9X@���@��@�dZ@�33@��@��+@�5?@��^@���@���@��7@�?}@���@�bN@�1'@�1@��@���@�l�@�"�@��y@��+@�5?@�{@��@���@��@�G�@�X@�O�@�&�@���@�j@�9X@�1'@��@�  @�  @�  @�b@�b@�;@�1@�@�w@��@K�@�@~�y@~v�@~V@~5?@~{@}�h@}�@|�@|�/@|�j@|��@|1@{t�@{S�@z�H@z~�@z-@y�#@y��@yhs@y7L@x�`@x�u@xbN@xA�@xA�@x1'@w�;@w;d@w
=@v��@v5?@u�@p�u@ko@c�@^�+@W
=@O�w@HbN@B~�@;dZ@6��@0r�@,�@';d@!G�@I�@�y@��@��@
��@5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aʡ�Aɇ+A��A��/A��A��/Ağ�A�C�A��mAÙ�A�M�A�A��A��A�A�n�A��!A��A���A�/A�ƨA�E�A���A��A�=qA���A�$�A�v�A�A�-A���A��+A��A�;dA�z�A���A���A���A�7LA�+A�9XA�5?A���A�"�A��A��7A�1'A�I�A�&�A�
=A��RA�^5A��mA���A�n�A�A�ȴA��A��A�
=A��^A��7A�ffA�?}A��A��A��;A���A�ZA�&�A��A�~�A�;dA���A��A���A��A�K�A��PA���A���A���A�ZA��;A�p�A���A��A�~�A��/A�oA�"�A��jA�+A�$�A�n�A�jA���A|�yAy�AwS�AtI�Ar�9Aq�mAq"�ApM�Ao��AnȴAm�Aj^5Ag�AdZAcl�Aa��Aa�A`z�A_�mA_�A]��AZ�DAXbNAW�TAU?}AR�uAR(�AQ�TAQXAO��AN�yAN-AM�AK��AKG�AJ��AI�AI�-AH��AG��AG"�AFE�AEhsADQ�AC��ABȴAA��A@A�A>�A>$�A=VA<jA;�PA:I�A9�mA8E�A7ƨA6�A5ƨA4r�A3�7A2jA0��A/�mA.r�A-�;A-XA,�RA*�A)+A'��A& �A$��A$1'A#K�A"ĜA"��A"bA!K�A ȴA VA��A�RA�
A|�A��A��A�!A�A�TAl�A{A��An�A(�A��A�AoA=qA�A1A�-AXA�+A�-A�`A9XA��AS�A��AE�AS�A5?A�wA
�`A
VA	C�A �A;dA�mA+An�A�FAVAffAl�A �9A �@�;d@��@��@�1'@�"�@�G�@�"�@���@�&�@�@�@�
=@��@�@�"�@�@��/@��@���@��@�+@�-@��`@�@���@�9X@�@�;d@��@���@�  @�K�@ޏ\@���@ܛ�@���@ڟ�@��T@ش9@�\)@Ցh@ԣ�@�  @Ұ!@��@��@ΰ!@��@�G�@�dZ@��@���@��@ʰ!@ɉ7@���@ǝ�@���@�Q�@�@���@�&�@��@�bN@� �@��@�C�@��@�n�@�@��@�&�@�I�@�o@�$�@�?}@�bN@��P@�;d@���@��^@���@��+@��j@��@��@�7L@� �@�|�@�K�@��!@��@���@��7@�Ĝ@���@��R@�$�@��@�G�@���@��-@��@�Ĝ@�Q�@��@��P@�M�@���@��R@��#@���@�x�@�/@���@���@�(�@�l�@�;d@�
=@��@��+@��@�p�@�j@��@�l�@�~�@��^@��7@��@���@���@���@���@�~�@�E�@��-@��@�1@��@��@��@�v�@�5?@��T@���@�Ĝ@�r�@�bN@�Q�@�A�@�b@��@��@��R@�5?@���@���@��@�O�@��@���@�Z@��@��@�l�@�;d@�o@��@��!@�n�@�$�@���@�x�@�7L@�%@��@��j@���@��D@�9X@���@��@�dZ@�33@��@��+@�5?@��^@���@���@��7@�?}@���@�bN@�1'@�1@��@���@�l�@�"�@��y@��+@�5?@�{@��@���@��@�G�@�X@�O�@�&�@���@�j@�9X@�1'@��@�  @�  @�  @�b@�b@�;@�1@�@�w@��@K�@�@~�y@~v�@~V@~5?@~{@}�h@}�@|�@|�/@|�j@|��@|1@{t�@{S�@z�H@z~�@z-@y�#@y��@yhs@y7L@x�`@x�u@xbN@xA�@xA�@x1'@w�;@w;d@w
=@v��@v5?@u�@p�u@ko@c�@^�+@W
=@O�w@HbN@B~�@;dZ@6��@0r�@,�@';d@!G�@I�@�y@��@��@
��@5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�#B�B��B��B��BŢB�wB�qB�wB��B��B�qB�qB�wB�qB�RB�B��B�BjBXBO�BE�B'�B�B1BJBuB�BO�Bn�BXBjB�B�%B�B|�Bv�Bn�BaHB\)BVBM�BJ�BI�BE�B@�B0!B-B+B&�B�B�B�B�BhBbB\BPBB  B��B��B��B��B��B��B�B�B�fB�BB�B��B��BŢB�B��B�\B|�BiyBVB1'BB�)B�dB��BiyBF�B!�B
�sB
�3B
��B
�=B
iyB
\)B
H�B
6FB
uB	��B	�TB	��B	ĜB	�wB	�XB	�3B	�B	��B	��B	�VB	|�B	jB	bNB	XB	T�B	Q�B	O�B	K�B	B�B	1'B	#�B	�B	oB	
=B	
=B	
=B	PB	JB	DB		7B	B	  B��B��B��B��B��B��B��B�B�yB�B�B�B�B�`B�HB�;B�)B�B�B��B��BÖBƨBƨB��B�qB�^B�FB�B��B��B��B��B��B��B�oB�JB�B{�Bz�B{�Bz�B}�Bz�Bv�Bv�Bt�Bq�Bm�BhsBgmBgmBiyBl�BjBiyBjBbNB\)B[#BZBYB[#B\)BXBR�BM�BM�BK�BJ�BH�BF�BD�BC�BA�B?}B=qB:^B8RB5?B49B1'B.B+B'�B%�B#�B!�B�B�B�B�B�B�B�B�BuBhBbBPBJBJBDB
=B
=B	7B+B%BBBBBBBBB  B��B  BBBBBBBBBB%BB	7BJBJBJB	7B	7B
=BDBJBVBbBbBbBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B"�B"�B%�B'�B(�B-B/B0!B0!B2-B1'B/B.B.B.B.B2-B49B6FB9XB:^B:^B9XB9XB>wB@�BB�BE�BP�BZB]/BaHBaHBgmBjBm�Bl�Bo�Bq�Bs�Bv�Bx�Bz�Bz�B|�B~�B�B�B�%B�+B�1B�=B�\B�oB�uB��B��B��B��B��B��B�B�B�'B�3B�?B�9B�9B�LB�RB�^B�}BBÖBĜBŢB��B��B��B��B��B��B��B�B�)B�NB�ZB�`B�sB�B�B�B�B��B��B��B��B	  B	B	B	1B	
=B	VB	bB	uB	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	)�B	.B	1'B	49B	8RB	8RB	9XB	9XB	;dB	>wB	A�B	C�B	D�B	D�B	F�B	H�B	J�B	K�B	M�B	P�B	S�B	W
B	YB	ZB	\)B	^5B	aHB	bNB	bNB	e`B	gmB	gmB	hsB	jB	l�B	m�B	m�B	o�B	q�B	s�B	u�B	w�B	y�B	{�B	{�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�JB	�VB	�bB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�RB	��B	�TB	�B
B
hB
�B
'�B
1'B
7LB
?}B
E�B
I�B
Q�B
XB
^5B
e`B
gmB
k�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B��B��B��BőB�gB�_B�fB�uB�tB�^B�aB�cB�aB�=B��B��B��BjkBW�BO�BE�B'�B�BB7B_BhBO�Bn�BW�BjnB�B�B��B|�Bv�Bn�Ba2B\BU�BM�BJ�BI�BE�B@mB0	B,�B*�B&�B�B�BuBqBPBNBDB7BB��B��B��B��B��B��B��B�B�fB�MB�+B��B��B̴BňB��B��B�@B|�Bi_BU�B1B�B�B�MB�oBi^BF�B!�B
�WB
�B
��B
�"B
iaB
\B
H�B
62B
_B	��B	�@B	��B	ċB	�eB	�EB	�!B	�B	��B	��B	�DB	|�B	joB	b=B	XB	T�B	Q�B	O�B	K�B	B�B	1B	#�B	�B	aB	
/B	
-B	
0B	AB	;B	5B		+B	B��B��B��B��B��B��B��B��B�B�nB�rB�B�B�xB�TB�>B�.B�B�	B��B��B˹BÉBƜBƜB�|B�fB�SB�;B�B��B��B��B��B��B��B�eB�?B�B{�Bz�B{�Bz�B}�Bz�Bv�Bv�Bt�Bq�Bm�BhiBgdBgdBipBl�BjtBirBjuBbEB\B[BZBYB[B\#BXBR�BM�BM�BK�BJ�BH�BF�BD�BC�BA�B?uB=hB:WB8JB59B4/B1B.B*�B'�B%�B#�B!�B�B�B�BwBoBjBdB]BnBEB>B/B'BBB!B
B
B	B	BB�B�B�B�B�B�B�BB��B��B��B �B�B�B �B �B �B
B�BBB�B	BDB&BBB	-B	B
B:B%B2B>BYB>BkBcB�B�B�B�B�B�BuB�B|B�B�B�B�B�B�B�B�B�B �B �B"�B"�B%�B'�B(�B-B/B0B0B2"B1B/B.B.B.B.B2 B4.B69B9MB:QB:RB9KB9LB>jB@wBB�BE�BP�BZB]!Ba:Ba9Bg_BjqBm�Bl|Bo�Bq�Bs�Bv�Bx�Bz�Bz�B|�B~�B�B�
B�B�B�!B�-B�LB�^B�bB�vB��B��B��B��B��B�B�B�B�"B�-B�&B�&B�9B�@B�LB�kB�|BÃBĉBŏB̹B��B��B��B��B��B��B�B�B�;B�DB�KB�_B�hB�wB�B�B��B��B��B��B��B	�B	B	B	
'B	AB	LB	_B	hB	nB	B	�B	�B	!�B	#�B	$�B	&�B	)�B	-�B	1B	4 B	8:B	88B	9>B	9?B	;JB	>`B	ApB	C{B	D�B	D�B	F�B	H�B	J�B	K�B	M�B	P�B	S�B	V�B	X�B	ZB	\B	^B	a0B	b3B	b3B	eEB	gRB	gRB	h[B	jeB	lpB	mxB	mtB	o�B	q�B	s�B	u�B	w�B	y�B	{�B	{�B	|�B	}�B	~�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�*B	�1B	�<B	�GB	�GB	�WB	�ZB	�_B	�cB	�iB	�pB	�wB	�yB	�yB	�yB	�~B	��B	��B	��B	��B	��B	�6B	˪B	�4B	�B
�B
JB
�B
'�B
1B
7*B
?^B
E�B
I�B
Q�B
W�B
^B
e>B
gMB
kdB
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.28 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708132016053117081320160531170813  AO  ARCAADJP                                                                    20140721230829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230829  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230829  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170813  IP                  G�O�G�O�G�O�                