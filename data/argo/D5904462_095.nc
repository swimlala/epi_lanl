CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-27T20:17:45Z AOML 3.0 creation; 2016-08-07T21:51:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151227201745  20160807145125  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               _A   AO  5287_9017_095                   2C  D   APEX                            6529                            072314                          846 @׉_���1   @׉`O�
h@/�G�z��d�9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    _A   B   B   @�ff@���A��A��AA��A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B���B�  B�33B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD���D�@ D��3D���D� D�I�D�vfD�ɚD�3D�I�D�i�D��fD���D�9�D�|�D�ɚD� D�@ D�fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33A��A$  AH��Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��B��B��3B��fB��3B��fB��B��fB��3B��fB��fBȳ3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3C��Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D#3D�3D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��DegDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk#3Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�3Dy�3D�3D�NfD���D��3D�fD�X D���D�� D�!�D�X D�x D���D�3D�H Dڋ3D�� D�fD�NfD��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�z�A҇+A�~�A҉7A҃A҅A҅Aҏ\Aҕ�Aҗ�Aҟ�Aң�Aҥ�Aҡ�Aң�Aң�Aҡ�Aҡ�Aң�Aҥ�Aҡ�Aң�Aҩ�AҮAҲ-A�A��A�33A�I�A�S�A�^5A�hsAӓuA�;dAԁAԇ+Aԥ�Aԉ7A�1Aҝ�A�?}AЕ�A�Q�A�p�A���A�\)A´9A���A�p�A���A��mA��A��A��A�?}A��uA���A�ĜA�=qA�bA���A�M�A�1A�A�A��#A��A�`BA�|�A��;A�oA��\A���A�-A���A���A��wA��HA��+A��A���A�VA��;A�bNA�?}A�$�A���A��!A�l�A��A��jA�oA��/A}x�A{AxI�At�Aq�
AkXAdA`bA^z�A[&�AYO�AWG�ATz�AQO�AOt�AN �AK�PAI7LAFn�AC��AA��A@��A?�TA?A<1'A9��A8�A733A2�HA0�yA,~�A+p�A+�A*I�A&��A$  A#C�A$=qA$�A#"�A#�7A!�-A�HA/A��A�wA��Ap�AffA�jA��A�PA�\AffA��AZA��A��A��A�FA��A��A�AA�mA��A�PAC�A
�A	��A	t�A�/Al�A�#A�HAQ�A��A?}A"�A �yA 5?@��w@�;d@�hs@�ff@�S�@�V@���@�o@�^@�r�@�w@�x�@�h@�C�@��@���@�w@㕁@�33@��@�r�@��@�r�@�M�@�Q�@�r�@�j@�Z@�  @�  @��D@�j@�V@�z�@���@�z�@��;@�+@���@�M�@ܴ9@�
=@�E�@��@���@٩�@�X@�&�@�&�@�z�@�9X@�E�@��@�V@Ѻ^@�/@Гu@�I�@�(�@���@υ@�@Ο�@�M�@�5?@�E�@�V@�{@͉7@���@̛�@̓u@̣�@̣�@̋D@�9X@���@��m@˝�@�l�@ʟ�@�^5@�J@���@��#@�&�@ț�@��m@���@��@ź^@Ł@ċD@�A�@�I�@���@�x�@�Ĝ@�  @���@��F@���@��P@�|�@�\)@�+@�@��H@�ȴ@��R@��!@�~�@��@���@�7L@��w@��@�ff@��@��@��D@�bN@�Q�@�(�@� �@�1@��
@���@�"�@��H@��@���@�ȴ@��R@���@���@�v�@���@��7@���@���@��D@�bN@��;@��R@��@��^@���@�hs@�G�@�/@��@���@��@�A�@�1@�ƨ@��@�33@�E�@�J@��#@��^@���@���@���@���@���@���@��7@�`B@�O�@�7L@��@��j@�I�@�ƨ@�\)@�33@���@�E�@�$�@��@�p�@�V@��u@�Q�@��@�K�@��!@�ff@�=q@�{@��@���@���@��^@���@�%@�I�@��F@�S�@�;d@���@��!@��+@�^5@�@�`B@���@�r�@��m@�|�@�"�@��@���@�E�@��^@�hs@�O�@�G�@�G�@�G�@�&�@�j@���@�ƨ@��P@�\)@�o@��!@�V@��^@�%@���@�Ĝ@�Ĝ@�Ĝ@��j@��9@�z�@�b@��@��P@�|�@�dZ@�C�@�+@��y@�-@�O�@���@��D@�1'@� �@���@���@�33@��R@��\@�M�@�-@�{@��#@���@�X@�1'@�|�@�
=@��!@���@��R@�ȴ@���@���@��R@��+@�ff@��@�"�@���@��!@�5?@���@��@��@��@��@��T@��T@���@���@��@���@��j@��9@�z�@�9X@��@��w@�t�@�"�@��@��R@�~�@�@���@���@���@��h@�X@�%@�Ĝ@�1'@�x�@~��@pb@cdZ@ZJ@P�`@H�@@�u@;@4�@,j@'
=@ bN@�m@r�@�m@r�@��@	hs@5?@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�x�A�z�A҇+A�~�A҉7A҃A҅A҅Aҏ\Aҕ�Aҗ�Aҟ�Aң�Aҥ�Aҡ�Aң�Aң�Aҡ�Aҡ�Aң�Aҥ�Aҡ�Aң�Aҩ�AҮAҲ-A�A��A�33A�I�A�S�A�^5A�hsAӓuA�;dAԁAԇ+Aԥ�Aԉ7A�1Aҝ�A�?}AЕ�A�Q�A�p�A���A�\)A´9A���A�p�A���A��mA��A��A��A�?}A��uA���A�ĜA�=qA�bA���A�M�A�1A�A�A��#A��A�`BA�|�A��;A�oA��\A���A�-A���A���A��wA��HA��+A��A���A�VA��;A�bNA�?}A�$�A���A��!A�l�A��A��jA�oA��/A}x�A{AxI�At�Aq�
AkXAdA`bA^z�A[&�AYO�AWG�ATz�AQO�AOt�AN �AK�PAI7LAFn�AC��AA��A@��A?�TA?A<1'A9��A8�A733A2�HA0�yA,~�A+p�A+�A*I�A&��A$  A#C�A$=qA$�A#"�A#�7A!�-A�HA/A��A�wA��Ap�AffA�jA��A�PA�\AffA��AZA��A��A��A�FA��A��A�AA�mA��A�PAC�A
�A	��A	t�A�/Al�A�#A�HAQ�A��A?}A"�A �yA 5?@��w@�;d@�hs@�ff@�S�@�V@���@�o@�^@�r�@�w@�x�@�h@�C�@��@���@�w@㕁@�33@��@�r�@��@�r�@�M�@�Q�@�r�@�j@�Z@�  @�  @��D@�j@�V@�z�@���@�z�@��;@�+@���@�M�@ܴ9@�
=@�E�@��@���@٩�@�X@�&�@�&�@�z�@�9X@�E�@��@�V@Ѻ^@�/@Гu@�I�@�(�@���@υ@�@Ο�@�M�@�5?@�E�@�V@�{@͉7@���@̛�@̓u@̣�@̣�@̋D@�9X@���@��m@˝�@�l�@ʟ�@�^5@�J@���@��#@�&�@ț�@��m@���@��@ź^@Ł@ċD@�A�@�I�@���@�x�@�Ĝ@�  @���@��F@���@��P@�|�@�\)@�+@�@��H@�ȴ@��R@��!@�~�@��@���@�7L@��w@��@�ff@��@��@��D@�bN@�Q�@�(�@� �@�1@��
@���@�"�@��H@��@���@�ȴ@��R@���@���@�v�@���@��7@���@���@��D@�bN@��;@��R@��@��^@���@�hs@�G�@�/@��@���@��@�A�@�1@�ƨ@��@�33@�E�@�J@��#@��^@���@���@���@���@���@���@��7@�`B@�O�@�7L@��@��j@�I�@�ƨ@�\)@�33@���@�E�@�$�@��@�p�@�V@��u@�Q�@��@�K�@��!@�ff@�=q@�{@��@���@���@��^@���@�%@�I�@��F@�S�@�;d@���@��!@��+@�^5@�@�`B@���@�r�@��m@�|�@�"�@��@���@�E�@��^@�hs@�O�@�G�@�G�@�G�@�&�@�j@���@�ƨ@��P@�\)@�o@��!@�V@��^@�%@���@�Ĝ@�Ĝ@�Ĝ@��j@��9@�z�@�b@��@��P@�|�@�dZ@�C�@�+@��y@�-@�O�@���@��D@�1'@� �@���@���@�33@��R@��\@�M�@�-@�{@��#@���@�X@�1'@�|�@�
=@��!@���@��R@�ȴ@���@���@��R@��+@�ff@��@�"�@���@��!@�5?@���@��@��@��@��@��T@��T@���@���@��@���@��j@��9@�z�@�9X@��@��w@�t�@�"�@��@��R@�~�@�@���@���@���@��h@�X@�%@�ĜG�O�@�x�@~��@pb@cdZ@ZJ@P�`@H�@@�u@;@4�@,j@'
=@ bN@�m@r�@�m@r�@��@	hs@5?@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	VB	VB	XB	W
B	W
B	VB	VB	T�B	T�B	T�B	VB	W
B	XB	XB	W
B	W
B	T�B	T�B	T�B	T�B	VB	VB	VB	XB	XB	YB	�B	��B	�XB	��B	�B
VB
)�B
P�B
��B
��B
ǮB
�ZB$�BJ�B�=B�!B�yB(�B/B)�BZBjBr�B�B�DB�PBt�Bk�Bz�B�hB�7B{�Br�B^5BB�B49B0!B33B@�B>wBuB�^B�uB�B+B�B��B�/B��B�}B�B�7Br�B\)BG�B0!B&�B�BDB
��B
�B
�sB
�BB
��B
�-B
��B
�B
C�B
1'B
�B	��B	�)B	�B	�{B	}�B	p�B	^5B	Q�B	?}B	,B	�B	uB	JB	B��B�sB�5B�B�B�)B�B�
B��B��B��B��B�^BÖBƨB��B��B��B��B�)B��B	B��B	�B	JB�B�B�B	  B	�B	!�B	&�B	'�B	+B	/B	-B	,B	9XB	F�B	O�B	`BB	ffB	iyB	e`B	[#B	\)B	bNB	gmB	gmB	e`B	cTB	_;B	YB	W
B	S�B	N�B	F�B	A�B	A�B	F�B	F�B	G�B	G�B	G�B	F�B	D�B	C�B	@�B	>wB	<jB	B�B	B�B	>wB	;dB	=qB	8RB	.B	&�B	 �B	�B	�B	�B	�B	�B	-B	5?B	5?B	:^B	=qB	?}B	@�B	B�B	I�B	P�B	T�B	VB	ZB	]/B	gmB	t�B	}�B	|�B	~�B	~�B	~�B	|�B	|�B	|�B	{�B	{�B	{�B	{�B	�B	�7B	�7B	�7B	�7B	�=B	�=B	�=B	�=B	�DB	�PB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�?B	�FB	�LB	�RB	�^B	�dB	�qB	�qB	�qB	��B	��B	��B	��B	�}B	�}B	�}B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�`B	�fB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
	7B

=B
PB
bB
bB
bB
bB
hB
hB
hB
oB
hB
\B
VB
VB
VB
\B
bB
bB
bB
hB
hB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
49B
9XB
E�B
J�B
P�B
XB
]/B
bNB
gmB
k�B
n�B
q�B
v�B
y�B
}�B
�B
�B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	U�B	U�B	W�B	V�B	V�B	U�B	U�B	T�B	T�B	T�B	U�B	V�B	W�B	W�B	V�B	V�B	T�B	T�B	T�B	T�B	U�B	U�B	U�B	W�B	W�B	YB	��B	��B	�>B	��B	�B
9B
)�B
P�B
�sB
�^B
ǍB
�:B$�BJ�B�B��B�VB(�B.�B)�BY�Bj[Br�B��B� B�-Bt�Bk`Bz�B�@B�B{�Br�B^
BBfB4B/�B3
B@ZB>OBJB�6B�KB�dB*�B�B��B�B̣B�SB��B�Br�B[�BG�B/�B&�B�BB
��B
�^B
�JB
�B
��B
�B
��B
��B
CmB
1 B
ZB	��B	�B	��B	�WB	}�B	p�B	^B	Q�B	?ZB	+�B	�B	UB	'B	 �B��B�SB�B��B� B�B��B��B��BκB˧B�bB�>B�wBƆBʠB��B̬B��B�B��B	 �B��B	aB	$B�B�uB�B��B	�B	!�B	&�B	'�B	*�B	.�B	,�B	+�B	9.B	FB	O�B	`B	f:B	iNB	e3B	Z�B	[�B	b"B	g@B	g@B	e6B	c+B	_B	X�B	V�B	S�B	N�B	F}B	AaB	AaB	F~B	F|B	G�B	G�B	G�B	F~B	DsB	ClB	@YB	>LB	<?B	BgB	BfB	>LB	;9B	=FB	8'B	-�B	&�B	 �B	�B	�B	�B	�B	�B	,�B	5B	5B	:3B	=BB	?RB	@WB	BbB	I�B	P�B	T�B	U�B	Y�B	]B	g>B	t�B	}�B	|�B	~�B	~�B	~�B	|�B	|�B	|�B	{�B	{�B	{�B	{�B	��B	�B	�B	�
B	�	B	�B	�B	�B	�B	�B	� B	�'B	�4B	�7B	�KB	�XB	�jB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�/B	�6B	�@B	�AB	�AB	�VB	�XB	�WB	�QB	�KB	�MB	�KB	�XB	�_B	�mB	�wB	ɉB	̙B	ϫB	ϪB	дB	бB	бB	бB	ѺB	ѺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�!B	� B	�!B	�!B	�(B	�&B	�'B	�(B	�+B	�/B	�/B	�,B	�,B	�-B	�,B	�3B	�.B	�3B	�.B	�/B	�2B	�2B	�3B	�3B	�3B	�3B	�3B	�5B	�3B	�9B	�9B	�:B	�:B	�;B	�@B	�GB	�GB	�GB	�MB	�JB	�KB	�LB	�QB	�PB	�RB	�SB	�TB	�YB	�YB	�XB	�^B	�]B	�^B	�]B	�dB	�dB	�_B	�iB	�qB	�zB	�|B	�|B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

	B
B
/B
-B
.B
1B
2B
2B
1B
9B
3B
$B
!B
"B
B
*B
,B
,B
-B
5B
3B
;B
6B
RB
YB
VB
WB
^B
kB
qB
sB
qB
sB
qB
qB
qB
rB
uB
wB
wB
yB
pB
jB
iB
iB
sB
xB
wB
sB
pB
jB
lB
jB
lB
jB
rB
qB
mG�O�B
qB
#�B
)�B
4B
9 B
EnB
J�B
P�B
W�B
\�B
bB
g6B
kPB
naB
qsB
v�B
y�B
}�B
��B
��B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451252016080714512520160807145125  AO  ARCAADJP                                                                    20151227201745    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151227201745  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151227201745  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145125  IP                  G�O�G�O�G�O�                