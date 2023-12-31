CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-16T10:25:15Z AOML 3.0 creation; 2016-08-07T21:36:42Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160116102515  20160807143643  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               _A   AO  5286_8897_095                   2C  D   APEX                            6531                            072314                          846 @׎IDt�M1   @׎I�b�C@2��E���c>��n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    _A   B   B   @9��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt��Dy�fD�  D�6fD�FfD���D��fD�,�D���D��fD�3D�L�D��3D�ɚD�fD�FfD�s3D��3D���D�C3D�\�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @AG�@��
@��
A�A!�AA�Aa�A�(�A���A���A���A���A���A���A���B �GBz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bx�GB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�p�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��DtDt��Dt�{Dy�D��D�:=D�J=D�ФD��=D�0�D��qD��=D�
D�P�D��
D��qD�
=D�J=D�w
D��
D��qD�G
D�`�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AɶFAɉ7A�O�A�/A��A�bA�%A���A��A��A�  A�VA�bA�bA��Aț�A�oAǟ�A�n�A�I�A�
=A�|�A��A���A��A��yA�n�AƃAƛ�A�^5A�`BA�^5A��Aš�A�=qA�5?AÏ\A��A�-A��!A�1'A��!A��#A��FA�A��/A��FA�O�A�r�A�~�A��uA��A�{A�p�A�;dA��A�$�A��`A�l�A��\A���A���A�x�A�G�A��
A��A��A�VA�XA�ȴA�^5A���A��A�oA�oA��A�"�A��A���A���A���A�I�A��
A��9A��A�\)A�ȴA���A���A���A�A���A���A�JA� �A�n�A�5?A�5?A���A���A��A�`BA�=qA�AwAu7LAshsAp�uAkXAhbNAdĜAap�A\A�AW�wAV�AT�AQ��AM��AL�DAJZAH�AG��AFE�AAx�A?��A=�hA<�\A<JA8��A6�A4A1x�A0�DA/��A/��A/G�A/%A-��A,jA)�A'�A&��A%�#A$�A#��A#��A"�RA!��A!�A!`BA!�A �+A�!A��A��A�TA"�A��AffA7LAA��AVAjA=qAO�AA�A"�Az�A��A
�jA	`BA1'A"�A%A�RAbAhsA%A�A�A�9AS�A Ĝ@��P@���@���@��P@�^5@�&�@��@�x�@���@��R@���@�G�@��@�A�@�@�Z@�7@�1'@�F@�K�@�-@畁@���@�9X@�!@��@�(�@�=q@��`@�"�@�|�@�l�@��@�1'@��@�"�@���@��/@Л�@�ƨ@��@��@�?}@�Ĝ@�bN@�b@�|�@ʧ�@��@���@ɉ7@�%@�z�@�I�@�(�@��m@�n�@�  @�S�@�+@�@�b@�\)@���@��@��D@�ƨ@���@���@��P@�|�@�K�@���@�~�@��@��7@�/@�A�@�C�@���@���@��j@��j@��`@���@�  @�|�@�C�@�E�@���@�%@���@�z�@�Z@�S�@�@���@�?}@�&�@���@�Q�@�b@��;@��w@�C�@�v�@�$�@��^@��7@�7L@��D@�1'@���@���@�
=@��R@��\@�5?@��@�x�@�O�@�V@���@��j@��D@���@�ƨ@���@��@�t�@��m@��
@�l�@�C�@���@�ff@���@�7L@�z�@��F@��@�dZ@�+@���@�ȴ@��@��!@�~�@�V@�5?@�{@�J@�@��-@�hs@�/@��@��`@��@���@�bN@�9X@��@��P@�C�@�
=@��y@���@���@��@��h@��@�z�@�1'@��F@���@��\@�~�@���@��\@�v�@�@��T@��#@�p�@�z�@�1'@��@��@�1@��@��w@��@�;d@���@�~�@�E�@�E�@�^5@���@�hs@�?}@�?}@�&�@�Ĝ@��j@�/@���@�o@���@�ȴ@���@���@��y@���@���@�ff@�5?@��@��@��-@�hs@��/@��9@��D@�j@�A�@��@��P@���@�~�@�=q@�{@��@��^@��h@�hs@�O�@��D@�ƨ@�t�@�dZ@�\)@�C�@��H@�n�@�^5@�^5@�M�@�@��-@���@��@��j@�r�@�bN@�Z@�Z@��@���@�t�@�\)@��R@�M�@���@���@��h@�?}@��@���@��/@��j@�z�@�9X@��@��m@��@�|�@�;d@��R@���@�~�@�-@�@���@��u@�r�@�Q�@�9X@��@��@K�@
=@
=@~�@~��@~v�@~{@}��@}@}@}@}@}�-@}p�@|z�@xA�@p1'@i&�@ahs@Xr�@R-@J�H@B~�@=�-@6ȴ@.{@)&�@#dZ@5?@1'@��@%@`B@	%@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  AɶFAɉ7A�O�A�/A��A�bA�%A���A��A��A�  A�VA�bA�bA��Aț�A�oAǟ�A�n�A�I�A�
=A�|�A��A���A��A��yA�n�AƃAƛ�A�^5A�`BA�^5A��Aš�A�=qA�5?AÏ\A��A�-A��!A�1'A��!A��#A��FA�A��/A��FA�O�A�r�A�~�A��uA��A�{A�p�A�;dA��A�$�A��`A�l�A��\A���A���A�x�A�G�A��
A��A��A�VA�XA�ȴA�^5A���A��A�oA�oA��A�"�A��A���A���A���A�I�A��
A��9A��A�\)A�ȴA���A���A���A�A���A���A�JA� �A�n�A�5?A�5?A���A���A��A�`BA�=qA�AwAu7LAshsAp�uAkXAhbNAdĜAap�A\A�AW�wAV�AT�AQ��AM��AL�DAJZAH�AG��AFE�AAx�A?��A=�hA<�\A<JA8��A6�A4A1x�A0�DA/��A/��A/G�A/%A-��A,jA)�A'�A&��A%�#A$�A#��A#��A"�RA!��A!�A!`BA!�A �+A�!A��A��A�TA"�A��AffA7LAA��AVAjA=qAO�AA�A"�Az�A��A
�jA	`BA1'A"�A%A�RAbAhsA%A�A�A�9AS�A Ĝ@��P@���@���@��P@�^5@�&�@��@�x�@���@��R@���@�G�@��@�A�@�@�Z@�7@�1'@�F@�K�@�-@畁@���@�9X@�!@��@�(�@�=q@��`@�"�@�|�@�l�@��@�1'@��@�"�@���@��/@Л�@�ƨ@��@��@�?}@�Ĝ@�bN@�b@�|�@ʧ�@��@���@ɉ7@�%@�z�@�I�@�(�@��m@�n�@�  @�S�@�+@�@�b@�\)@���@��@��D@�ƨ@���@���@��P@�|�@�K�@���@�~�@��@��7@�/@�A�@�C�@���@���@��j@��j@��`@���@�  @�|�@�C�@�E�@���@�%@���@�z�@�Z@�S�@�@���@�?}@�&�@���@�Q�@�b@��;@��w@�C�@�v�@�$�@��^@��7@�7L@��D@�1'@���@���@�
=@��R@��\@�5?@��@�x�@�O�@�V@���@��j@��D@���@�ƨ@���@��@�t�@��m@��
@�l�@�C�@���@�ff@���@�7L@�z�@��F@��@�dZ@�+@���@�ȴ@��@��!@�~�@�V@�5?@�{@�J@�@��-@�hs@�/@��@��`@��@���@�bN@�9X@��@��P@�C�@�
=@��y@���@���@��@��h@��@�z�@�1'@��F@���@��\@�~�@���@��\@�v�@�@��T@��#@�p�@�z�@�1'@��@��@�1@��@��w@��@�;d@���@�~�@�E�@�E�@�^5@���@�hs@�?}@�?}@�&�@�Ĝ@��j@�/@���@�o@���@�ȴ@���@���@��y@���@���@�ff@�5?@��@��@��-@�hs@��/@��9@��D@�j@�A�@��@��P@���@�~�@�=q@�{@��@��^@��h@�hs@�O�@��D@�ƨ@�t�@�dZ@�\)@�C�@��H@�n�@�^5@�^5@�M�@�@��-@���@��@��j@�r�@�bN@�Z@�Z@��@���@�t�@�\)@��R@�M�@���@���@��h@�?}@��@���@��/@��j@�z�@�9X@��@��m@��@�|�@�;d@��R@���@�~�@�-@�@���@��u@�r�@�Q�@�9X@��@��@K�@
=@
=@~�@~��@~v�@~{@}��@}@}@}@}@}�-@}p�G�O�@xA�@p1'@i&�@ahs@Xr�@R-@J�H@B~�@=�-@6ȴ@.{@)&�@#dZ@5?@1'@��@%@`B@	%@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
M�B
R�B
_;B
ffB
o�B
�+B
��B
��B�B/B>wBO�BffBjBiyBk�By�B�BĜB�B�B�B��BPB�B{B<jBQ�BH�BP�B\)B^5BjBv�B�B�{B��B��BB��B��B�jB��B�B�VB�=B�PB�B�Bv�Bl�BffBbNBbNB`BBYBH�B=qB2-B#�B�BoB+B��B�B��B��B�BɺB�!B�hB�B|�B~�Bl�BP�B?}B#�B1B
�B
ɺB
��B
z�B
gmB
XB
;dB
.B
�B
1B	�B	�TB	�B	��B	�qB	�B	~�B	m�B	aHB	N�B	,B	�B��B�5B�qB��B��B��B�DB~�By�Bu�Bo�BjBbNBYBVBXBVBR�BR�BP�BXBaHBdZBffBgmBgmBgmBjBjBn�Bo�Bo�Bp�Br�Bt�Bs�Bz�B� B�B�B�=B�hB�7B�B}�B� B�=B�Bx�Bq�Br�Bq�BjBcTBbNBaHB`BB_;BaHBffBiyBiyBiyBk�Bl�Bm�Bq�Bq�Bp�Bn�Bl�BiyBjBk�Bn�Bn�Bp�Br�Bu�Bx�B�7B�hB��B��B��B��B��B��B��B��B��B�oB�{B��B�{B�\B�7B�B�+B�B�B� By�Bq�BhsBhsBhsBdZBgmBp�Bq�Bo�Bq�Bs�Br�Bt�B{�B~�B� B�B�B�+B�=B�JB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�LB�RB�RB�RB�XB�^B�jBƨB��B��B��B��B�
B�
B�B�B�5B�5B�)B�/B�TB�`B�yB�B�B�B�B��B��B	B	%B	%B	1B	PB	uB	�B	�B	�B	�B	�B	�B	�B	#�B	(�B	,B	/B	0!B	49B	6FB	8RB	;dB	>wB	A�B	F�B	H�B	J�B	N�B	P�B	P�B	Q�B	T�B	ZB	^5B	iyB	k�B	l�B	l�B	l�B	l�B	n�B	o�B	r�B	v�B	x�B	x�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�PB	�VB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�FB	�LB	�LB	�RB	�^B	�^B	�dB	�jB	�qB	�wB	�}B	ĜB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	�B	�TB	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
hB
bB
�B
�B
&�B
.B
33B
9XB
A�B
E�B
K�B
T�B
ZB
_;B
cTB
hsB
m�B
p�B
t�B
w�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
M�B
R�B
_6B
feB
o�B
�(B
��B
��B�B/B>oBO�Bf\BjwBipBk{By�B��BĔB�B�B�B��BDB|BpB<`BQ�BH�BP�B\B^,BjvBv�B�B�oB�~B��BB��B��B�bB�{B�B�NB�5B�EB�B�Bv�Bl�Bf\BbBBbAB`7BYBH�B=fB2 B#�B�BfBB��B�B��B��B�BɰB�B�]B��B|�B~�Bl�BP�B?tB#�B)B
�B
ɯB
��B
z�B
ggB
XB
;`B
.B
�B
/B	�B	�PB	� B	��B	�mB	�B	~�B	m�B	aKB	N�B	,B	�B��B�=B�yB�B��B��B�LBBy�Bu�Bo�Bj�BbZBY#BVBXBVBR�BR�BP�BXBaRBdeBfpBgwBgxBgvBj�Bj�Bn�Bo�Bo�Bp�Br�Bt�Bs�Bz�B�
B�B�"B�EB�qB�>B�B}�B�
B�CB�$Bx�Bq�Br�Bq�Bj�Bc\BbXBaQB`IB_DBaQBfrBi�Bi�Bi�Bk�Bl�Bm�Bq�Bq�Bp�Bn�Bl�Bi�Bj�Bk�Bn�Bn�Bp�Br�Bu�Bx�B�?B�nB��B��B��B��B��B��B��B��B��B�tB��B��B��B�dB�=B�"B�3B�#B�B�By�Bq�Bh|BhyBhzBd^BguBp�Bq�Bo�Bq�Bs�Br�Bt�B{�BB�B�B�B�1B�AB�OB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�NB�PB�TB�SB�UB�ZB�aB�lBƨB��B��B��B��B�
B�B�B�B�7B�6B�*B�1B�VB�_B�xB�B�B�B�B��B��B	B	%B	%B	1B	NB	rB	B	�B	�B	�B	�B	�B	�B	#�B	(�B	,B	/B	0B	46B	6DB	8QB	;cB	>sB	A�B	F�B	H�B	J�B	N�B	P�B	P�B	Q�B	T�B	ZB	^1B	itB	k�B	l�B	l�B	l�B	l�B	n�B	o�B	r�B	v�B	x�B	x�B	z�B	{�B	|�B	}�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�&B	�+B	�6B	�LB	�PB	�]B	�]B	�cB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�!B	�,B	�@B	�DB	�BB	�LB	�WB	�XB	�^B	�cB	�lB	�qB	�xB	ĔB	śB	ǥB	ǨB	ʻB	��B	��B	��B	��B	��B	�B	�JB	�[B	�^B	�^B	�eB	�bB	�iB	�tB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
B
B
B
 �B
B

B
B
B
B
&B
	.B

4B

0B
<B
:B
9B
BB
FB
EB
FB
GB
DB
LB
LB
KB
MB
SB
QB
QB
QG�O�B
XB
�B
�B
&�B
.B
3+B
9MB
AB
E�B
K�B
T�B
ZB
_.B
cJB
hgB
m�B
p�B
t�B
w�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436432016080714364320160807143643  AO  ARCAADJP                                                                    20160116102515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160116102515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160116102515  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143643  IP                  G�O�G�O�G�O�                