CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-10-24T22:00:53Z AOML 3.0 creation; 2016-06-01T00:08:21Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20141024220053  20160531170821  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               _A   AO  4055_7112_095                   2C  D   APEX                            5374                            041511                          846 @�''@�1   @�(Y��@9����l��d�`A�71   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    _A   A   A   @�ff@���A   A   A@  A`  A�  A���A�  A�  A�  A�  A���A�  B   B  B  B��B��B(  B0  B8  B@  BH  BQ��BX  B`ffBf  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD��D�9�D��3D���D�3D�L�D���D��3D�3D�<�D�� D��3D� D�P D�l�D���D�3D�C3D�\�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@ƸRA��A$��AD��Ad��A�z�A�G�A�z�A�z�A�z�A�z�A�G�A�z�B=qB	=qB=qB
>B �B)=qB1=qB9=qBA=qBI=qBR�BY=qBa��Bg=qBq=qBy=qB���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4=D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�
Dy�=D��D�C�D��D�ÆD�D�V�D���D��D�D�F�D���D��D��D�Y�D�v�D�ֹD�D�MD�f�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�$�A�(�A�$�A�+A�33A�9XA�5?A�1'A�1'A�7LA�9XA�9XA�7LA�=qA�?}A�7LA�9XA�;dA�A�A�=qA�C�A�G�A�G�A�G�A�5?A�XA�K�A���A�1A�I�A���A��^A���A�ffA��FA���A���A��A�r�A���A�M�A���A���A�p�A�O�A��-A�G�A��A���A���A�K�A�C�A��DA�1'A��A���A��A�33A�
=A�K�A�^5A��A���A�ĜA�z�A�C�A���A��A� �A�  A���A� �A�ƨA��A��
A��A��\A�C�A��A��A�E�A�-A�bA���A���A���A��A�oA���A�A�A��A�Q�A���A�A�A�{A�5?A�x�A��A���A~A|$�Az�+Ax��Av�Av�Au�-AtM�Ar�/ArAq|�Ao�AjȴAi?}Ah1'Agp�Af�!Af$�Ae+Ac�#Aa��A`5?A_p�A^JA]oA\n�A\1A[�#A[�AZ��AY�TAW��AWp�AW/AVĜAT�9AR�9AR-AP�\ANȴAL��AK�-AJ�`AIAHffAG�hAF��AE�mAE�AC7LABVAA��A@r�A?�7A>-A=p�A<�A<1A:jA8��A8JA77LA6��A6=qA5\)A5&�A4�A4��A4E�A3`BA2jA1+A0��A0v�A0E�A/`BA.�uA.JA-�A-7LA,�jA+��A*��A)�A)+A(�A(A�A'dZA&�+A&I�A%�wA$��A#�A!G�A �+A�#A&�AJAG�AE�A�AbA��A�AAI�A;dA�!A1'A�;AS�A��AG�AQ�A��AffA��A�AQ�AO�A�\Ax�A
�A
�!A	��A	"�A�jAffAoAbAA�AE�A|�A�yA�7A �y@�l�@���@�v�@�1@���@�p�@���@�@�@�`B@�Ĝ@�^5@�@�(�@��m@�x�@��@�~�@�h@�I�@��@�M�@���@�(�@�dZ@ڸR@�V@�{@���@�V@ץ�@�K�@��@ա�@��@��@��@��@��@�33@�J@�&�@�S�@�X@�A�@��@î@�v�@�hs@��j@��@�ȴ@�V@��@��@�@�hs@���@��@��@���@�  @�S�@��\@�V@�J@�@��@�1@��@��m@��@��w@��P@�\)@�33@�@��@�p�@�hs@�%@��D@�t�@���@��u@���@�33@���@��h@�p�@�%@���@��@�  @��@�~�@��@���@���@�`B@��@���@���@�=q@��h@���@�1'@���@�@�E�@���@��`@��@��@��@�dZ@�\)@�K�@�33@���@���@�^5@��-@��@�z�@�(�@�  @��@��
@��w@��@�ȴ@�{@���@�p�@�?}@�&�@��@���@��@��
@�^5@���@��9@���@�r�@�I�@� �@���@��;@��@�\)@�ȴ@��T@��-@�hs@�%@��D@� �@��m@��@��@�l�@�C�@�
=@�~�@�$�@��T@���@�O�@�G�@�&�@���@�1'@��F@���@�t�@�33@�K�@�"�@��!@�v�@�^5@�M�@�5?@�{@��@��^@�x�@�hs@�?}@���@��`@��@��u@��u@�z�@�bN@�I�@��w@�|�@�o@��\@��@��@�x�@�G�@��`@��/@�Ĝ@��9@���@�I�@�w@�@�@+@~$�@}�h@z�@y�#@yG�@x��@x�u@xbN@x1'@x �@w�;@w�w@w�P@wK�@w
=@v�y@u��@u`B@u�@t��@tZ@tI�@s��@sC�@sC�@s@so@r~�@qG�@pĜ@pr�@pA�@p �@pb@m�@d�j@_��@U��@Q&�@K��@CC�@=p�@6E�@3o@.�@(��@$��@��@~�@5?@�F@��@
�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�$�A�(�A�$�A�+A�33A�9XA�5?A�1'A�1'A�7LA�9XA�9XA�7LA�=qA�?}A�7LA�9XA�;dA�A�A�=qA�C�A�G�A�G�A�G�A�5?A�XA�K�A���A�1A�I�A���A��^A���A�ffA��FA���A���A��A�r�A���A�M�A���A���A�p�A�O�A��-A�G�A��A���A���A�K�A�C�A��DA�1'A��A���A��A�33A�
=A�K�A�^5A��A���A�ĜA�z�A�C�A���A��A� �A�  A���A� �A�ƨA��A��
A��A��\A�C�A��A��A�E�A�-A�bA���A���A���A��A�oA���A�A�A��A�Q�A���A�A�A�{A�5?A�x�A��A���A~A|$�Az�+Ax��Av�Av�Au�-AtM�Ar�/ArAq|�Ao�AjȴAi?}Ah1'Agp�Af�!Af$�Ae+Ac�#Aa��A`5?A_p�A^JA]oA\n�A\1A[�#A[�AZ��AY�TAW��AWp�AW/AVĜAT�9AR�9AR-AP�\ANȴAL��AK�-AJ�`AIAHffAG�hAF��AE�mAE�AC7LABVAA��A@r�A?�7A>-A=p�A<�A<1A:jA8��A8JA77LA6��A6=qA5\)A5&�A4�A4��A4E�A3`BA2jA1+A0��A0v�A0E�A/`BA.�uA.JA-�A-7LA,�jA+��A*��A)�A)+A(�A(A�A'dZA&�+A&I�A%�wA$��A#�A!G�A �+A�#A&�AJAG�AE�A�AbA��A�AAI�A;dA�!A1'A�;AS�A��AG�AQ�A��AffA��A�AQ�AO�A�\Ax�A
�A
�!A	��A	"�A�jAffAoAbAA�AE�A|�A�yA�7A �y@�l�@���@�v�@�1@���@�p�@���@�@�@�`B@�Ĝ@�^5@�@�(�@��m@�x�@��@�~�@�h@�I�@��@�M�@���@�(�@�dZ@ڸR@�V@�{@���@�V@ץ�@�K�@��@ա�@��@��@��@��@��@�33@�J@�&�@�S�@�X@�A�@��@î@�v�@�hs@��j@��@�ȴ@�V@��@��@�@�hs@���@��@��@���@�  @�S�@��\@�V@�J@�@��@�1@��@��m@��@��w@��P@�\)@�33@�@��@�p�@�hs@�%@��D@�t�@���@��u@���@�33@���@��h@�p�@�%@���@��@�  @��@�~�@��@���@���@�`B@��@���@���@�=q@��h@���@�1'@���@�@�E�@���@��`@��@��@��@�dZ@�\)@�K�@�33@���@���@�^5@��-@��@�z�@�(�@�  @��@��
@��w@��@�ȴ@�{@���@�p�@�?}@�&�@��@���@��@��
@�^5@���@��9@���@�r�@�I�@� �@���@��;@��@�\)@�ȴ@��T@��-@�hs@�%@��D@� �@��m@��@��@�l�@�C�@�
=@�~�@�$�@��T@���@�O�@�G�@�&�@���@�1'@��F@���@�t�@�33@�K�@�"�@��!@�v�@�^5@�M�@�5?@�{@��@��^@�x�@�hs@�?}@���@��`@��@��u@��u@�z�@�bN@�I�@��w@�|�@�o@��\@��@��@�x�@�G�@��`@��/@�Ĝ@��9@���@�I�@�w@�@�@+@~$�@}�h@z�@y�#@yG�@x��@x�u@xbN@x1'@x �@w�;@w�w@w�P@wK�@w
=@v�y@u��@u`B@u�@t��@tZ@tI�@s��@sC�@sC�@s@so@r~�@qG�@pĜ@pr�@pA�@p �@pb@m�@d�j@_��@U��@Q&�@K��@CC�@=p�@6E�@3o@.�@(��@$��@��@~�@5?@�F@��@
�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�fB�fB�fB�fB�fB�`B�`B�fB�fB�fB�`B�`B�`B�fB�`B�`B�fB�`B�`B�ZB�ZB�`B�`B�`B�5B�B�/B��B��B��B��B��B�oB�B}�Bw�Bs�Bm�BjBe`B`BB\)BO�BC�BA�B;dB(�B�B�BbB+B  B�B�B�sB�mB�NB�B��BB��B|�Bt�Bq�BaHBN�B>wB9XB6FB.B�B\B��B��B�B�mB�BB��B�wB�?B��B��B��B��B��B�VB}�Bp�B`BBI�B?}B2-B�B\B1BB
�B
ȴB
��B
�%B
l�B
[#B
M�B
>wB
+B
&�B
!�B
�B
	7B
B	��B	�`B	ÖB	�dB	�RB	�9B	�B	��B	��B	��B	�DB	�B	}�B	v�B	p�B	l�B	jB	hsB	ffB	bNB	\)B	S�B	P�B	N�B	K�B	@�B	9XB	6FB	/B	%�B	�B	�B	�B	{B	bB	JB	1B	B	B��B��B�B�B�yB�fB�ZB�TB�;B�#B�B��B��B��B��B��B��B��B��B��BȴBB�}B�wB�qB�^B�9B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B�{B�hB�PB�+B�B�B~�B{�Bw�Bt�Bp�Bm�Bk�BjBiyBgmBe`BbNB`BB^5B\)BYBVBQ�BO�BM�BJ�BI�BG�BD�BA�B?}B>wB=qB;dB:^B8RB7LB5?B33B2-B1'B/B-B+B'�B$�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B{BuBhBoBoBhBhBoBhBoBoBuB{B{B{B{B�B�B�B�B�B�B�B"�B#�B#�B(�B)�B)�B'�B-B1'B2-B2-B9XB<jB>wB?}BD�BE�BF�BF�BF�BF�BF�BH�BK�BM�BO�BP�BR�BR�BS�BR�BS�BT�BXBYBYBYBXBXBXB^5BaHBaHB`BB`BB`BBbNB`BB]/B^5BdZBgmBjBl�Bo�Bq�Br�Bt�Bw�Bz�B{�B{�B|�B}�B~�B� B�B�B�B� B�B�B�+B�DB�PB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�FB�RB�qBĜBȴB��B��B��B��B��B��B�B�
B�/B�BB�HB�HB�NB�ZB�mB�B�B�B�B�B��B��B��B	B	%B	
=B	PB	\B	hB	oB	uB	{B	�B	�B	�B	�B	#�B	#�B	$�B	'�B	.B	1'B	2-B	33B	5?B	9XB	@�B	D�B	G�B	H�B	I�B	J�B	K�B	L�B	M�B	P�B	P�B	R�B	W
B	XB	ZB	\)B	\)B	]/B	]/B	^5B	dZB	ffB	iyB	l�B	p�B	q�B	t�B	t�B	v�B	x�B	y�B	y�B	z�B	}�B	�B	�B	�B	�B	�%B	�+B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�HB	��B
%B
oB
 �B
(�B
49B
9XB
=qB
F�B
K�B
T�B
ZB
_;B
bNB
hsB
m�B
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�UB�WB�UB�WB�UB�RB�PB�VB�RB�VB�PB�PB�RB�VB�RB�PB�VB�PB�PB�LB�IB�PB�PB�PB�'B��B�BʯB��B��B��B��B�[B�
B}�Bw�Bs�Bm|BjjBeJB`*B\BO�BC|BAqB;HB(�B�BvBHBB��B�B�oB�WB�QB�0B��BʧB�sB��B|�Bt�Bq�Ba*BN�B>ZB99B6'B-�B}B<B��B��B�B�PB�%B��B�WB�!B��B��B��B��B�jB�8B}�Bp�B`&BI�B?_B2B�B=BB�B
�B
ȕB
��B
�B
lqB
[
B
M�B
>^B
*�B
&�B
!�B
hB
	B
 �B	��B	�JB	�B	�NB	�=B	�$B	�B	��B	��B	�~B	�-B	��B	}�B	v�B	p�B	lvB	jnB	h_B	fOB	b9B	\B	S�B	P�B	N�B	K�B	@qB	9FB	64B	/	B	%�B	�B	�B	�B	kB	RB	7B	"B	B	 �B��B��B�B�B�iB�VB�IB�EB�-B�B� B��B��B��B��B��B��B��B̾BʲBȥBB�oB�eB�`B�NB�+B� B�B�B�B��B��B��B��B��B��B��B��B�rB�nB�YB�AB�B�B��B~�B{�Bw�Bt�Bp�Bm�BkvBjsBikBgaBeQBbAB`6B^'B\BY	BU�BQ�BO�BM�BJ�BI�BG�BD�BA}B?oB>kB=fB;XB:SB8FB7AB5B3'B2!B0�B/B-B*�B'�B$�B#�B �B�B�B�B�B|B{B�B�BfBuBYBtBWBiBBBHBFBBBBBIB@BaBcBgBoBTBoBoBuB�B�B�BsB�B�B"�B#�B#�B(�B)�B)�B'�B- B1B2B2B9IB<XB>gB?lBD�BE�BF�BF�BF�BF�BF�BH�BK�BM�BO�BP�BR�BR�BS�BR�BS�BT�BW�BYBYBYBW�BW�BW�B^"Ba6Ba9B`1B`/B`0Bb;B`0B]B^#BdDBgZBjnBlvBo�Bq�Br�Bt�Bw�Bz�B{�B{�B|�B}�B~�B�B��B��B��B�B��B��B�B�1B�<B�`B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�-B�;B�ZBĆBȝBʩBͻB��B��B��B��B��B��B�B�*B�/B�/B�5B�AB�TB�hB�qB�B�B��B��B��B��B	 �B	
B	
 B	2B	AB	NB	RB	XB	`B	B	�B	�B	�B	#�B	#�B	$�B	'�B	-�B	1B	2B	3B	5!B	9<B	@hB	D~B	G�B	H�B	I�B	J�B	K�B	L�B	M�B	P�B	P�B	R�B	V�B	W�B	ZB	\B	\	B	]B	]B	^B	d<B	fGB	iYB	lnB	p�B	q�B	t�B	t�B	v�B	x�B	y�B	y�B	z�B	}�B	��B	��B	��B	��B	�B	�	B	�B	�#B	�)B	�1B	�4B	�7B	�<B	�;B	�BB	�BB	�CB	�MB	�UB	�\B	�tB	�{B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	��B
B
LB
 �B
(�B
4B
94B
=NB
F�B
K�B
T�B
Y�B
_B
b+B
hOB
mmB
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708212016053117082120160531170821  AO  ARCAADJP                                                                    20141024220053    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141024220053  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141024220053  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170821  IP                  G�O�G�O�G�O�                