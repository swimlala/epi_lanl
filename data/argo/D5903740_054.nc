CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:33Z AOML 3.0 creation; 2016-06-01T00:08:14Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230833  20160531170814  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               6A   AO  4055_7112_054                   2C  D   APEX                            5374                            041511                          846 @ֵ&�1   @ֵ'����@9��-V�c��t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    6A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D�fD  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dts3Dy��D�  D�FfD���D�� D��fD�,�D���D�ɚD��fD�6fD�s3D��3D�  D�<�DږfD�� D�  D�<�D�vfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @K�@���@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bp�RBy�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DRD�RD�D��D�D�RD�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#RD#�RD$RD$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DXRDX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy��D��D�O\D���D���D��\D�5�D���D�ҐD��\D�?\D�|)D��)D��D�E�Dڟ\D���D��D�E�D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�|�A�~�AԃAԉ7AԓuAԑhAԏ\Aԏ\AԍPAԕ�AԑhAԍPAԍPA�v�A�I�A���A�;dA��A�VA�(�A̴9A�?}AɃAȺ^A�jAƗ�A�E�A�^5Aú^A��mA��RA�9XA�1'A��RA��A���A�A���A�oA��A�XA�9XA�{A�oA���A��A�|�A�  A���A�`BA�{A��mA�\)A��HA���A�jA���A��`A�O�A�ȴA��A�1'A��uA�33A�A���A�1A�ZA�1'A���A�~�A�v�A�bNA�K�A�;dA�$�A�ƨA��+A�5?A�7LA�-A�v�A�A�x�A�1A�C�A��A��#A��A�n�A��A�Q�A�;dA��+A�ƨA���A���A��HA��wA�l�A��A��jA�S�A��A���A�p�A�mA{�Ay�-Ax�\Av�At�Aq��An��Am�AlI�Ak��Ai�Af�uAfQ�Ae�mAe��Ad�Aa&�A^�A^^5A\z�AYx�AX��AV$�ATM�AR^5AP{ANI�AM"�AL~�AKoAJ5?AG�AF=qAE�
AEp�ADz�ACK�AB~�AAXA@��A@bA>�9A=�mA<�/A< �A;+A9%A8^5A7��A6�A6  A4�`A3G�A1�;A1��A0��A.��A-�#A,I�A*��A)&�A(��A'�^A'`BA&ĜA%�7A$bNA#G�A"�A"~�A!��A �A 1'A�Ax�AbNA�A�A�#A+A�A�A+A��A�wA
=AM�AbA��A�7A?}A
=A��A&�A%AQ�A�mAx�A�A5?A�TA
�A
bNA	��AȴA�A�/A�+A�A�mA|�A��AE�A��A�AƨA?}A n�A J@�o@�z�@�@�t�@��#@�Ĝ@�z�@�(�@�|�@�7@@�j@���@��@��@��@�G�@蛦@�  @�n�@��@�A�@���@ۥ�@ٺ^@�I�@׮@�
=@Ԭ@�E�@�&�@Гu@Ο�@�O�@�V@���@̼j@�
=@ʰ!@��@�/@���@��@��m@���@��T@��@�  @��@���@��/@��;@��P@�=q@�p�@�%@�Z@�ƨ@���@�@�Q�@��w@��@�C�@���@��7@�/@��9@�(�@��@���@���@��@�p�@�hs@�O�@�G�@���@��D@�r�@�bN@�A�@�b@��@�S�@��R@�@�p�@�&�@��`@�Q�@��@�t�@��!@�`B@��@��@���@�E�@���@��D@� �@�33@�=q@��^@�p�@�`B@�?}@�%@��@��9@�r�@�(�@��@�E�@�$�@�@���@�@�x�@�O�@�7L@�%@�Ĝ@�(�@�1@��@�@���@��+@�-@�p�@��9@�Q�@���@�ƨ@���@�\)@�@��@��!@�~�@�$�@��7@��/@��@��@�S�@��@��R@���@��+@�n�@�5?@���@�hs@�&�@�bN@��@��@�\)@�K�@�C�@���@��@�7L@���@�Z@�b@��m@��F@�dZ@�o@��!@�n�@�V@�$�@���@��-@��@�`B@�7L@��@��@���@�Ĝ@���@��@�j@�bN@�Z@�A�@�  @�ƨ@���@�;d@��@��@���@��R@�ff@�$�@�J@��T@��^@��-@�p�@�&�@��@��@��D@�j@�Q�@� �@�;@�P@
=@~v�@~{@}@}`B@}�@|�/@|9X@{�
@{C�@{"�@z��@zn�@z=q@y��@y�7@x��@xQ�@w��@w�P@w\)@w;d@v��@v�y@v�y@vȴ@v��@v�+@v$�@u�@u@u�-@u�h@u/@t�/@tj@s�
@s��@st�@sC�@r��@r=q@q��@qx�@q�@q%@n@f{@\�/@U��@P�`@I7L@C33@<�D@7��@1��@,�/@(bN@ b@�
@V@�F@�P@��@	G�@O�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�v�A�|�A�~�AԃAԉ7AԓuAԑhAԏ\Aԏ\AԍPAԕ�AԑhAԍPAԍPA�v�A�I�A���A�;dA��A�VA�(�A̴9A�?}AɃAȺ^A�jAƗ�A�E�A�^5Aú^A��mA��RA�9XA�1'A��RA��A���A�A���A�oA��A�XA�9XA�{A�oA���A��A�|�A�  A���A�`BA�{A��mA�\)A��HA���A�jA���A��`A�O�A�ȴA��A�1'A��uA�33A�A���A�1A�ZA�1'A���A�~�A�v�A�bNA�K�A�;dA�$�A�ƨA��+A�5?A�7LA�-A�v�A�A�x�A�1A�C�A��A��#A��A�n�A��A�Q�A�;dA��+A�ƨA���A���A��HA��wA�l�A��A��jA�S�A��A���A�p�A�mA{�Ay�-Ax�\Av�At�Aq��An��Am�AlI�Ak��Ai�Af�uAfQ�Ae�mAe��Ad�Aa&�A^�A^^5A\z�AYx�AX��AV$�ATM�AR^5AP{ANI�AM"�AL~�AKoAJ5?AG�AF=qAE�
AEp�ADz�ACK�AB~�AAXA@��A@bA>�9A=�mA<�/A< �A;+A9%A8^5A7��A6�A6  A4�`A3G�A1�;A1��A0��A.��A-�#A,I�A*��A)&�A(��A'�^A'`BA&ĜA%�7A$bNA#G�A"�A"~�A!��A �A 1'A�Ax�AbNA�A�A�#A+A�A�A+A��A�wA
=AM�AbA��A�7A?}A
=A��A&�A%AQ�A�mAx�A�A5?A�TA
�A
bNA	��AȴA�A�/A�+A�A�mA|�A��AE�A��A�AƨA?}A n�A J@�o@�z�@�@�t�@��#@�Ĝ@�z�@�(�@�|�@�7@@�j@���@��@��@��@�G�@蛦@�  @�n�@��@�A�@���@ۥ�@ٺ^@�I�@׮@�
=@Ԭ@�E�@�&�@Гu@Ο�@�O�@�V@���@̼j@�
=@ʰ!@��@�/@���@��@��m@���@��T@��@�  @��@���@��/@��;@��P@�=q@�p�@�%@�Z@�ƨ@���@�@�Q�@��w@��@�C�@���@��7@�/@��9@�(�@��@���@���@��@�p�@�hs@�O�@�G�@���@��D@�r�@�bN@�A�@�b@��@�S�@��R@�@�p�@�&�@��`@�Q�@��@�t�@��!@�`B@��@��@���@�E�@���@��D@� �@�33@�=q@��^@�p�@�`B@�?}@�%@��@��9@�r�@�(�@��@�E�@�$�@�@���@�@�x�@�O�@�7L@�%@�Ĝ@�(�@�1@��@�@���@��+@�-@�p�@��9@�Q�@���@�ƨ@���@�\)@�@��@��!@�~�@�$�@��7@��/@��@��@�S�@��@��R@���@��+@�n�@�5?@���@�hs@�&�@�bN@��@��@�\)@�K�@�C�@���@��@�7L@���@�Z@�b@��m@��F@�dZ@�o@��!@�n�@�V@�$�@���@��-@��@�`B@�7L@��@��@���@�Ĝ@���@��@�j@�bN@�Z@�A�@�  @�ƨ@���@�;d@��@��@���@��R@�ff@�$�@�J@��T@��^@��-@�p�@�&�@��@��@��D@�j@�Q�@� �@�;@�P@
=@~v�@~{@}@}`B@}�@|�/@|9X@{�
@{C�@{"�@z��@zn�@z=q@y��@y�7@x��@xQ�@w��@w�P@w\)@w;d@v��@v�y@v�y@vȴ@v��@v�+@v$�@u�@u@u�-@u�h@u/@t�/@tj@s�
@s��@st�@sC�@r��@r=q@q��@qx�@q�@q%@n@f{@\�/@U��@P�`@I7L@C33@<�D@7��@1��@,�/@(bN@ b@�
@V@�F@�P@��@	G�@O�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�+B�+B�+B�+B�+B�%B�%B�+B�+B�+B�+B�+B�%B�B�Bw�Bl�BXB>wB&�B�B  B��B�B�ZB�ZB�NB�B��BɺBB�jB�3B��B��B��B�bB�DB�7B�+B�B}�B�%B�B�DB�BhsBp�BS�BK�BJ�BL�BM�BXBXBVBO�BE�BD�BC�B>wB8RB-B'�B$�B �B{B1BB��B�;B�BB�HB�;B�/B�)B�B��BŢB�B��B�=Bz�Br�Be`BT�B:^B�B1B��B�ZBɺB�?B��B�{B|�BhsBC�B+B!�BuB
�yB
��B
�B
w�B
P�B
5?B
{B
1B	��B	�B	�B	ŢB	�B	��B	��B	��B	�%B	x�B	u�B	q�B	m�B	dZB	K�B	>wB	8RB	+B	�B	uB	%B��B�B�NB�B��B��B��BƨB�}B�^B�RB�FB�9B�'B�!B�3B�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�JB�1B�B�B�B�B}�By�Bv�Bt�Bs�Bq�Bn�Bl�BjBiyBgmBdZBbNB_;B\)BZBVBR�BQ�BO�BM�BL�BK�BJ�BI�BI�BH�BF�BC�B?}B<jB;dB:^B8RB7LB5?B33B2-B0!B.B-B)�B(�B'�B&�B%�B#�B"�B"�B �B�B�B�B�B�B�BuBbB\BPBPBPBJB
=B+B+B+B1B+B%B%B%BBBBBBBBBBBBB+B+B%B1B	7B	7B	7B+B	7B	7B	7B	7B1B+BDBJBPBVB\BVB\B{B�B�B�B�B�B�B�B!�B!�B'�B(�B)�B+B-B2-B33B5?B7LB<jBA�BB�BC�BC�BC�BD�BD�BD�BF�BG�BG�BG�BG�BH�BI�BK�BN�BO�BP�BP�BS�BT�BW
BYB_;B_;B_;BcTBl�Bn�Bw�By�B~�B�B�1B�=B�=B�DB�PB�PB�VB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�RB�^B�jB�wB�}B��BÖBĜBŢBƨBȴB��B��B��B�B�/B�BB�HB�NB�NB�TB�ZB�mB�B�B��B��B��B��B	B	B	B	B	DB	VB	oB	uB	�B	�B	�B	�B	 �B	"�B	#�B	%�B	(�B	+B	,B	.B	0!B	1'B	33B	5?B	5?B	7LB	8RB	:^B	:^B	:^B	;dB	=qB	@�B	C�B	G�B	L�B	M�B	N�B	N�B	Q�B	VB	VB	YB	[#B	[#B	^5B	aHB	dZB	ffB	hsB	iyB	jB	l�B	n�B	p�B	s�B	v�B	x�B	z�B	|�B	~�B	� B	�B	�B	�1B	�7B	�DB	�JB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�wB	�B	�B
  B

=B
{B
�B
)�B
1'B
;dB
C�B
I�B
Q�B
XB
aHB
cTB
hsB
l�B
p�B
u�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�&B�B�#B�B�!B� B�B�B�$B�"B�"B�"B�"B�B�B�Bw�Bl�BXB>hB&�BzB��B��B�B�KB�JB�<B�B��BɧBB�ZB�!B��B��B�rB�NB�0B�"B�B��B}�B�B�B�0B��Bh[Bp�BS�BK�BJ�BL�BM�BW�BW�BU�BO�BE�BD�BC�B>bB8=B,�B'�B$�B �BfBBB��B�$B�)B�/B�#B�B�B��B��BňB��B�pB�$Bz�Br�BeDBT�B:CB�BB��B�@BɡB�#B��B�_B|�BhXBC}B*�B!�BZB
�`B
ʪB
��B
w�B
P�B
5)B
gB
B	��B	�zB	�B	ŎB	�B	��B	��B	�wB	�B	x�B	u�B	q�B	m�B	dKB	K�B	>hB	8BB	*�B	�B	fB	B��B�B�CB�B��B��BʷBƟB�qB�TB�HB�<B�.B�B�B�)B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�xB�]B�@B�&B�B�B�B��B}�By�Bv�Bt�Bs�Bq�Bn�Bl�BjxBioBgcBdMBbFB_2B\"BZBU�BR�BQ�BO�BM�BL�BK�BJ�BI�BI�BH�BF�BC�B?uB<bB;\B:SB8IB7CB5B3B2%B0B.B-B)�B(�B'�B&�B%�B#�B"�B"�B �B�B�B�B�BqBfBnB>B9B/B-B-B(B
BB	BBBBBBB�B�B�B�B�B�B�B�B�B�B�B�B!B#BBB	/B	-B	BB	B	,B	0B	B(BB;BCBHB2BQB2BSBVB�B�B�B�B�B�B�B!�B!�B'�B(�B)�B*�B-B2 B3'B52B7AB<^BA{BB�BC�BC�BC�BD�BD�BD�BF�BG�BG�BG�BG�BH�BI�BK�BN�BO�BP�BP�BS�BT�BV�BY
B_-B_+B_*BcEBlxBn�Bw�By�B~�B�B�#B�-B�-B�6B�AB�?B�DB�PB�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�?B�JB�WB�dB�iB�qBÃBĉBŎBƖBȠB˳B��B��B��B�B�-B�3B�9B�8B�@B�GB�YB�oB�B��B��B��B��B	�B	�B	�B	B	,B	@B	WB	]B	hB	pB	�B	�B	 �B	"�B	#�B	%�B	(�B	*�B	+�B	-�B	0
B	1B	3B	5&B	5'B	73B	8:B	:DB	:CB	:EB	;LB	=XB	@jB	CzB	G�B	L�B	M�B	N�B	N�B	Q�B	U�B	U�B	X�B	[	B	[	B	^B	a/B	d@B	fLB	hYB	i]B	jeB	loB	n~B	p�B	s�B	v�B	x�B	z�B	|�B	~�B	�B	��B	�B	�B	�B	�'B	�-B	�6B	�9B	�?B	�RB	�gB	�xB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�)B	�ZB	��B	�B	��B

B
^B
�B
)�B
1B
;DB
CwB
I�B
Q�B
W�B
a(B
c4B
hTB
llB
p�B
u�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.28 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708142016053117081420160531170814  AO  ARCAADJP                                                                    20140721230833    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230833  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230833  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170814  IP                  G�O�G�O�G�O�                