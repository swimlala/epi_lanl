CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:35Z AOML 3.0 creation; 2016-06-01T00:08:15Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230835  20160531170815  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               :A   AO  4055_7112_058                   2C  D   APEX                            5374                            041511                          846 @ֿd����1   @ֿe[ǿ�@:%�����cߍO�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    :A   A   A   @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B~  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D� D�P D�y�D��fD��D�FfD���D��3D���D�P D��fD��fD���D�Y�Dډ�D��3D�  D�<�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AC\)Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB=qB�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�B���BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNh�CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy��D��D�Y�D���D��RD��D�PRD���D��D��D�Y�D��RD��RD��D�c�Dړ�D��D�	�D�F�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�n�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�v�A�v�A�x�A�z�A�z�A�v�A�x�A�x�A�z�A�x�A�z�A�~�A�~�A�~�AсA�~�AсA�jA�`BA�Q�A� �A���A��mA�/A��A�&�A���A�1A�E�A�jA� �A��mA���A�$�A���A�dZA�I�A�oA���A�ZA���A�~�A���A�S�A�I�A�Q�A��A��A�|�A��DA�9XA��TA���A�A�A�ĜA�l�A�7LA�A��/A�  A�|�A�1A��RA���A�dZA�A�n�A���A�;dA��A���A�XA�1A�|�A�ƨA��mA�9XA���A�O�A�+A��`A�A���A�A�A��;A��
A��A��A��A�ĜA�/A�bA�dZA�I�A�7LA�x�A���A~r�A{�^Av�At��Ar$�Apr�An��AmoAk�^Aj9XAi�mAi�Ai�Aip�AioAh��AgK�Ad=qAadZA_�A_VA\ZAX��AW�AT��AP��AO�FAN�AMdZAL��AKS�AIl�AH1'AFv�AD��AC�ABĜAA��A?��A>�\A<5?A;O�A:^5A9��A9&�A7��A7�A61'A5�TA5�FA5K�A5
=A4��A2��A0�uA/;dA-l�A,bA+��A+�A*��A)��A(n�A(1'A'�A'�^A'%A&��A%�mA%oA$bA#\)A"�!A!�wA ffA?}A�\A��A�`A��A�DAA�A��A�wAO�A1A%A�+A{AȴAM�A�;A��Ax�A�DA�wA��A��AZA�A�^Ap�AjA`BA��A
��A
ZA	�^A	"�A�+A9XA��A��A��A�/A��A��AJA��A+A ��A @�@��@��/@��@��h@�/@��@��+@���@�@�J@��@���@�@�  @��y@�1'@��/@�u@�Z@��
@�S�@�5?@⟾@ߝ�@���@�$�@��@�@�+@Լj@��@��@҇+@�-@��@��@ϥ�@·+@̃@�33@ʸR@�{@�b@�l�@�S�@�o@�J@�9X@�C�@\@��#@���@��@�ȴ@�M�@�{@��7@�G�@���@��F@��+@�M�@�p�@�Ĝ@��@��@��@�G�@���@�^5@��@��@���@�%@�I�@�dZ@��@���@�J@�%@�9X@���@�K�@�@�v�@�=q@�M�@�=q@�$�@�@��7@�A�@���@�K�@���@�~�@��^@�/@�Ĝ@� �@�S�@��H@�~�@�ff@�=q@�J@��h@���@�j@�b@��F@��P@�K�@�n�@�J@��#@��-@��h@�`B@�1'@�o@��R@�E�@���@��@�j@��w@�l�@�@���@��#@�`B@�&�@�V@���@��u@�b@�|�@�K�@�33@��@�
=@��y@���@���@���@�p�@��`@��@�Z@�9X@��@�  @�ƨ@��P@�C�@�o@��@���@���@���@�v�@�M�@�E�@�=q@��@�/@�b@��y@�E�@�J@���@��@��@���@��^@���@�X@�X@�X@�X@�O�@��@�I�@�1@�l�@�
=@���@���@���@�ff@�5?@��@�{@���@��@��T@���@���@�7L@�?}@�&�@��/@�Ĝ@��9@�A�@�1@��@�ƨ@�\)@�"�@�@��@�ȴ@���@�v�@�^5@�V@��@���@��h@�p�@�hs@�G�@�7L@�%@���@���@��/@��j@���@��D@�r�@�Z@�A�@�9X@� �@�b@�;@��@�@l�@+@~��@~E�@~$�@}��@}`B@}`B@}�@|�j@|j@|1@{��@{33@{o@z�@zn�@zJ@y�7@t�@k��@e�-@^V@W+@N�@G�@C��@;��@5�@0Q�@(��@#t�@V@ƨ@l�@�F@+@  @�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�n�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�v�A�v�A�x�A�z�A�z�A�v�A�x�A�x�A�z�A�x�A�z�A�~�A�~�A�~�AсA�~�AсA�jA�`BA�Q�A� �A���A��mA�/A��A�&�A���A�1A�E�A�jA� �A��mA���A�$�A���A�dZA�I�A�oA���A�ZA���A�~�A���A�S�A�I�A�Q�A��A��A�|�A��DA�9XA��TA���A�A�A�ĜA�l�A�7LA�A��/A�  A�|�A�1A��RA���A�dZA�A�n�A���A�;dA��A���A�XA�1A�|�A�ƨA��mA�9XA���A�O�A�+A��`A�A���A�A�A��;A��
A��A��A��A�ĜA�/A�bA�dZA�I�A�7LA�x�A���A~r�A{�^Av�At��Ar$�Apr�An��AmoAk�^Aj9XAi�mAi�Ai�Aip�AioAh��AgK�Ad=qAadZA_�A_VA\ZAX��AW�AT��AP��AO�FAN�AMdZAL��AKS�AIl�AH1'AFv�AD��AC�ABĜAA��A?��A>�\A<5?A;O�A:^5A9��A9&�A7��A7�A61'A5�TA5�FA5K�A5
=A4��A2��A0�uA/;dA-l�A,bA+��A+�A*��A)��A(n�A(1'A'�A'�^A'%A&��A%�mA%oA$bA#\)A"�!A!�wA ffA?}A�\A��A�`A��A�DAA�A��A�wAO�A1A%A�+A{AȴAM�A�;A��Ax�A�DA�wA��A��AZA�A�^Ap�AjA`BA��A
��A
ZA	�^A	"�A�+A9XA��A��A��A�/A��A��AJA��A+A ��A @�@��@��/@��@��h@�/@��@��+@���@�@�J@��@���@�@�  @��y@�1'@��/@�u@�Z@��
@�S�@�5?@⟾@ߝ�@���@�$�@��@�@�+@Լj@��@��@҇+@�-@��@��@ϥ�@·+@̃@�33@ʸR@�{@�b@�l�@�S�@�o@�J@�9X@�C�@\@��#@���@��@�ȴ@�M�@�{@��7@�G�@���@��F@��+@�M�@�p�@�Ĝ@��@��@��@�G�@���@�^5@��@��@���@�%@�I�@�dZ@��@���@�J@�%@�9X@���@�K�@�@�v�@�=q@�M�@�=q@�$�@�@��7@�A�@���@�K�@���@�~�@��^@�/@�Ĝ@� �@�S�@��H@�~�@�ff@�=q@�J@��h@���@�j@�b@��F@��P@�K�@�n�@�J@��#@��-@��h@�`B@�1'@�o@��R@�E�@���@��@�j@��w@�l�@�@���@��#@�`B@�&�@�V@���@��u@�b@�|�@�K�@�33@��@�
=@��y@���@���@���@�p�@��`@��@�Z@�9X@��@�  @�ƨ@��P@�C�@�o@��@���@���@���@�v�@�M�@�E�@�=q@��@�/@�b@��y@�E�@�J@���@��@��@���@��^@���@�X@�X@�X@�X@�O�@��@�I�@�1@�l�@�
=@���@���@���@�ff@�5?@��@�{@���@��@��T@���@���@�7L@�?}@�&�@��/@�Ĝ@��9@�A�@�1@��@�ƨ@�\)@�"�@�@��@�ȴ@���@�v�@�^5@�V@��@���@��h@�p�@�hs@�G�@�7L@�%@���@���@��/@��j@���@��D@�r�@�Z@�A�@�9X@� �@�b@�;@��@�@l�@+@~��@~E�@~$�@}��@}`B@}`B@}�@|�j@|j@|1@{��@{33@{o@z�@zn�@zJ@y�7@t�@k��@e�-@^V@W+@N�@G�@C��@;��@5�@0Q�@(��@#t�@V@ƨ@l�@�F@+@  @�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B� B� B� B� B� B� B� B� B� B� B� B� B� B~�B� B~�B� B� B� B� B� B� B� B� B� B}�B|�Bz�Bq�B��B�B?}B+B$�B�B�B�B�B�B�B�B�B�B�B�B�BJBBB  B��B��B�B�HB�/B��BŢBBÖB�}B�jB�LB�'B�B��B��B��B�DB�Bv�B[#BI�B;dB1'B!�BhB�B��B�dB�!B��B��B�7Bv�Bk�BbNBXBYBbNBR�B-BPB
�B
�HB
��B
ÖB
��B
�jB
�!B
��B
�uB
�B
r�B
dZB
>wB
&�B
VB	�yB	�)B	��B	�wB	�3B	��B	��B	��B	�oB	�hB	�\B	�\B	�DB	�1B	z�B	ffB	S�B	J�B	B�B	0!B	�B	�B	B�B�B�fB�NB�5B��B��BƨB��B�jB�^B�RB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�DB�=B�1B�+B�B�B�B�B�B~�B|�Bz�Bx�Bu�Bs�Bq�Bm�BiyBgmBe`BbNBaHBaHB`BB_;B^5B]/B[#BW
BT�BS�BQ�BO�BM�BK�BH�BE�BC�BA�B@�B?}B>wB=qB<jB:^B7LB5?B2-B0!B/B.B,B+B)�B'�B&�B#�B�B�B�B�B�B�B�B�B�B{BoBbBbB\BVBPBDBDBDBDB
=B
=B	7B+B%B+B+B+B%BBBBBBBBBBBBBBBBBBB+B	7B	7B	7BJBPBJBDBDBVB\BbBbBhBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B!�B!�B!�B!�B$�B%�B&�B(�B,B/B2-B5?B8RB?}BB�BC�BC�BD�BI�BJ�BL�BO�BT�BXBYB[#B\)B^5BbNBffBiyBk�Bl�Bl�Bm�Bo�Br�Bt�Bv�Bx�Bx�By�B~�B�B�B�B�B�B�7B�bB�hB�{B��B��B��B��B��B��B��B�!B�3B�?B�FB�FB�XB�qB��BÖBĜBĜBŢBŢBǮB��B��B��B��B�B�B�B�B�B�#B�/B�;B�HB�TB�TB�ZB�ZB�`B�fB�fB�fB�sB�B��B	B	1B	
=B	
=B	DB	DB	JB	PB	VB	bB	bB	bB	bB	bB	hB	�B	�B	�B	!�B	#�B	#�B	%�B	'�B	)�B	+B	+B	,B	-B	-B	.B	0!B	49B	49B	49B	7LB	8RB	9XB	>wB	@�B	B�B	B�B	E�B	G�B	H�B	J�B	K�B	L�B	N�B	O�B	P�B	R�B	YB	[#B	\)B	\)B	^5B	_;B	aHB	aHB	bNB	cTB	dZB	ffB	gmB	hsB	iyB	jB	jB	k�B	l�B	m�B	n�B	n�B	p�B	q�B	t�B	v�B	w�B	z�B	|�B	|�B	}�B	� B	�B	�B	�%B	�1B	�1B	�7B	�JB	�VB	�{B	��B	ŢB	�)B	�B	��B

=B
�B
�B
.B
8RB
@�B
I�B
O�B
T�B
W
B
\)B
`BB
ffB
p�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B~�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�B�B~�B�B�B�B�B�B�B�B�B�B}�B|�Bz�Bq�B��B��B?eB*�B$�B�B�B�B�B�B�B�B�BxBuB�BnB3BB�B��B��B��B�iB�-B�B��BńB�qB�wB�`B�PB�0B�B��B��B��B��B�&B��Bv�B[
BI�B;IB1	B!�BMB�sBʢB�HB�B��B�|B�Bv�BkeBb2BW�BX�Bb1BR�B,�B3B
�B
�-B
͹B
�yB
�hB
�MB
�B
��B
�XB
��B
r�B
d>B
>ZB
&�B
>B	�cB	�B	ʭB	�bB	�B	��B	��B	�kB	�ZB	�QB	�GB	�EB	�1B	�B	z�B	fQB	S�B	J�B	B~B	0B	�B	wB	B�B�B�XB�?B�'B��B��BƗB�sB�\B�QB�CB�+B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�wB�ZB�BB�8B�0B�$B�B�B�B�B��B��B~�B|�Bz�Bx�Bu�Bs�Bq�Bm�BimBgdBeSBbCBa<Ba<B`7B_.B^'B]#B[BV�BT�BS�BQ�BO�BM�BK�BH�BE�BC�BA|B@vB?qB>lB=gB<]B:SB7AB52B2B/�B.�B.	B+�B*�B)�B'�B&�B#�B�B�B�B�BtB�B�BcBZBUBIB=B9B6B0B*B9BBB;B
B
B	BB�BBBB�B�B�BB�B�B�B�B�BBB�BB�B�BB�B�B�BB	,B	*B	B%BDB BB7B/BNBSBTBABaBlBuBWBtBtBaB}B�B�B�B~B�B�B�B�B�B!�B"�B!�B!�B!�B!�B$�B%�B&�B(�B+�B/
B2B5/B8BB?nBB~BC�BC�BD�BI�BJ�BL�BO�BT�BW�BYB[B\B^#Bb;BfRBifBkuBlwBlvBm�Bo�Br�Bt�Bv�Bx�Bx�By�B~�B��B��B��B��B��B�%B�MB�RB�eB�~B��B��B��B��B��B��B�B�B�'B�0B�0B�@B�YB�qBÀBĆBĄBŋBŋBǔB̵B��B��B��B��B��B��B��B�B�B�B�%B�.B�;B�=B�BB�CB�GB�LB�LB�LB�YB�xB��B	�B	B	
!B	
"B	(B	(B	0B	9B	<B	HB	HB	HB	EB	FB	MB	qB	~B	�B	!�B	#�B	#�B	%�B	'�B	)�B	*�B	*�B	+�B	,�B	,�B	-�B	0B	4B	4B	4B	70B	85B	9;B	>YB	@fB	BqB	BpB	E�B	G�B	H�B	J�B	K�B	L�B	N�B	O�B	P�B	R�B	X�B	[B	\B	\B	^B	_B	a+B	a+B	b1B	c9B	d=B	fGB	gNB	hWB	i\B	j`B	j`B	khB	lnB	mtB	n{B	nzB	p�B	q�B	t�B	v�B	w�B	z�B	|�B	|�B	}�B	�B	��B	��B	�B	�B	�B	�B	�*B	�5B	�\B	��B	ŀB	�B	�B	��B

B
mB
�B
-�B
8-B
@]B
I�B
O�B
T�B
V�B
\B
`B
fCB
p~B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708152016053117081520160531170815  AO  ARCAADJP                                                                    20140721230835    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230835  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230835  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170815  IP                  G�O�G�O�G�O�                