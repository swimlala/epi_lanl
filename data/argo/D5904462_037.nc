CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:41Z AOML 3.0 creation; 2016-08-07T21:51:15Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221441  20160807145115  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               %A   AO  5287_9017_037                   2C  D   APEX                            6529                            072314                          846 @�<���@1   @�<�y\��@1�^5?|��d�vȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    %A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D� D�<�D��fD��fD� D�C3D���D�� D��3D�,�D��fD��fD�  D�6fDچfD๚D���D�0 D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�B���B���B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn=Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�=Dy��D��D�F�D��RD��RD��D�MD���D���D��D�6�D��RD��RD�	�D�@RDڐRD�ÆD��D�9�D�v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A�$�A�&�A�&�A�(�A�(�A�-A�$�A��AЙ�A��AͲ-A��Ȁ\A�O�A���A�1'AʸRAʴ9A�S�A�hsAȟ�A�dZAȇ+Aȏ\A�+A�r�AƾwA��AŲ-AŃA�Q�A�I�A�ZA�1Aď\A��A�x�A°!A�A�\)A���A��A�-A�G�A�"�A��-A�bNA�I�A���A�O�A�l�A�ĜA���A�=qA�/A��PA���A���A���A�ȴA�%A�jA�33A��A�~�A�x�A��
A���A�hsA��FA�(�A���A�M�A��A��A���A��-A��A���A�?}A���A��A�E�A���A���A�n�A�~�A�?}A�A��7A��A�r�A�JA���A�K�A�1A��\A��uA��A~�\Az�RAx  Av�jAv �Au+Aq��AnM�Ak�wAi?}Ag��Af�HAe+Ac"�Aa��A_�A\�!AZ��AZ{AWO�ATn�ARANv�AK7LAG�AE��AD~�AA�PA?%A;t�A:A�A:A8�A6E�A1x�A.ffA-��A-�FA-\)A-
=A,z�A+/A*-A(�jA&��A$jA#XA!?}A�
A�/A�AVA�/AJA��A�TAbA�\A��AVA�RAƨAr�A�;A�AI�AQ�A^5A  A�yA�DA��A��A�A�AG�A
=A��A�A  A
ȴA	�A	��A	G�A	"�A	%A�yAffA�A�A�7A%A�DAVAt�A��A�A&�A�;@��m@��+@�j@���@���@���@��!@��@�z�@��;@�C�@���@���@�ff@�V@�-@�(�@��m@�\)@� �@�bN@�@��u@��@�@�9X@�@�$�@�%@�1@��H@��#@�&�@��@�D@�F@��@��@��`@蛦@�1@��@畁@�dZ@���@��@�9X@���@��@�\)@�
=@�@��@�v�@��@��`@���@߮@��@ߝ�@߅@�\)@���@�?}@��@�x�@�`B@�V@���@�"�@��@ָR@�@�V@ӶF@�l�@�dZ@�S�@�S�@�|�@�dZ@�@�$�@ѩ�@љ�@щ7@�X@�p�@ѡ�@�/@���@Ь@�9X@ϥ�@�"�@���@�n�@�5?@��#@�7L@��@�r�@��
@�S�@��H@ʟ�@�n�@��@Ɂ@�V@��@Ȭ@� �@Ǿw@�\)@��@�@ċD@�(�@þw@+@��#@��-@��@�/@���@�Z@�  @�S�@���@�ff@��#@���@�x�@�V@�I�@���@��P@��@�
=@�V@�$�@�p�@��u@�1@��@���@���@��\@��+@�ff@�p�@�V@�Q�@�l�@���@��^@��@��u@�I�@��@�l�@���@�E�@�@�9X@���@��@���@�5?@���@�hs@��7@�7L@���@�z�@�ƨ@�|�@�\)@�"�@�v�@���@��-@�x�@�G�@�Ĝ@�l�@�@��T@�`B@�O�@���@��9@�j@�A�@���@��R@�ff@�E�@�M�@�@���@�hs@��`@��@��@�o@��!@�n�@��@���@��@�7L@���@��@�hs@�G�@�V@��9@�1'@�ƨ@���@�v�@�-@��@���@��@���@��D@��@�9X@�(�@� �@���@��;@���@�;d@��@�@���@��+@�E�@�@�@��-@���@��h@�hs@�?}@���@��D@�A�@�  @��;@��w@��F@��F@���@���@��P@�S�@���@�p�@�?}@��@�V@���@��/@��/@���@��@�Q�@�(�@��@�1@���@���@��P@��@�t�@��@��y@��R@�n�@��@���@��#@��^@�p�@�G�@��+@�ƨ@|��@o�@i7L@a�@X  @N��@Ix�@A&�@;��@4�@/|�@-V@&��@#@ȴ@^5@?}@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A� �A�$�A�&�A�&�A�(�A�(�A�-A�$�A��AЙ�A��AͲ-A��Ȁ\A�O�A���A�1'AʸRAʴ9A�S�A�hsAȟ�A�dZAȇ+Aȏ\A�+A�r�AƾwA��AŲ-AŃA�Q�A�I�A�ZA�1Aď\A��A�x�A°!A�A�\)A���A��A�-A�G�A�"�A��-A�bNA�I�A���A�O�A�l�A�ĜA���A�=qA�/A��PA���A���A���A�ȴA�%A�jA�33A��A�~�A�x�A��
A���A�hsA��FA�(�A���A�M�A��A��A���A��-A��A���A�?}A���A��A�E�A���A���A�n�A�~�A�?}A�A��7A��A�r�A�JA���A�K�A�1A��\A��uA��A~�\Az�RAx  Av�jAv �Au+Aq��AnM�Ak�wAi?}Ag��Af�HAe+Ac"�Aa��A_�A\�!AZ��AZ{AWO�ATn�ARANv�AK7LAG�AE��AD~�AA�PA?%A;t�A:A�A:A8�A6E�A1x�A.ffA-��A-�FA-\)A-
=A,z�A+/A*-A(�jA&��A$jA#XA!?}A�
A�/A�AVA�/AJA��A�TAbA�\A��AVA�RAƨAr�A�;A�AI�AQ�A^5A  A�yA�DA��A��A�A�AG�A
=A��A�A  A
ȴA	�A	��A	G�A	"�A	%A�yAffA�A�A�7A%A�DAVAt�A��A�A&�A�;@��m@��+@�j@���@���@���@��!@��@�z�@��;@�C�@���@���@�ff@�V@�-@�(�@��m@�\)@� �@�bN@�@��u@��@�@�9X@�@�$�@�%@�1@��H@��#@�&�@��@�D@�F@��@��@��`@蛦@�1@��@畁@�dZ@���@��@�9X@���@��@�\)@�
=@�@��@�v�@��@��`@���@߮@��@ߝ�@߅@�\)@���@�?}@��@�x�@�`B@�V@���@�"�@��@ָR@�@�V@ӶF@�l�@�dZ@�S�@�S�@�|�@�dZ@�@�$�@ѩ�@љ�@щ7@�X@�p�@ѡ�@�/@���@Ь@�9X@ϥ�@�"�@���@�n�@�5?@��#@�7L@��@�r�@��
@�S�@��H@ʟ�@�n�@��@Ɂ@�V@��@Ȭ@� �@Ǿw@�\)@��@�@ċD@�(�@þw@+@��#@��-@��@�/@���@�Z@�  @�S�@���@�ff@��#@���@�x�@�V@�I�@���@��P@��@�
=@�V@�$�@�p�@��u@�1@��@���@���@��\@��+@�ff@�p�@�V@�Q�@�l�@���@��^@��@��u@�I�@��@�l�@���@�E�@�@�9X@���@��@���@�5?@���@�hs@��7@�7L@���@�z�@�ƨ@�|�@�\)@�"�@�v�@���@��-@�x�@�G�@�Ĝ@�l�@�@��T@�`B@�O�@���@��9@�j@�A�@���@��R@�ff@�E�@�M�@�@���@�hs@��`@��@��@�o@��!@�n�@��@���@��@�7L@���@��@�hs@�G�@�V@��9@�1'@�ƨ@���@�v�@�-@��@���@��@���@��D@��@�9X@�(�@� �@���@��;@���@�;d@��@�@���@��+@�E�@�@�@��-@���@��h@�hs@�?}@���@��D@�A�@�  @��;@��w@��F@��F@���@���@��P@�S�@���@�p�@�?}@��@�V@���@��/@��/@���@��@�Q�@�(�@��@�1@���@���@��P@��@�t�@��@��y@��R@�n�@��@���@��#@��^@�p�G�O�@��+@�ƨ@|��@o�@i7L@a�@X  @N��@Ix�@A&�@;��@4�@/|�@-V@&��@#@ȴ@^5@?}@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�bB
�hB
�oB
�hB
�bB
�uB
�uB
��B
��B
��B"�B�B�^B��B�;B2-BM�BbNBn�Bw�Bw�B��B�LB��B�/B�B  B1BPB�B�BuB#�B:^BJ�B[#B_;B_;BW
BN�BL�BN�BH�BD�BA�B<jBP�BQ�BH�B<jB$�B{BBVB�B#�B,B�B�B�B��B�LBÖBŢB��BĜB�3B�\B�BiyB\)BVBF�BH�BQ�BN�BB�B.BhB�;B�}B��B�PBy�Bk�BXB?}B0!BDBB
��B
��B
�sB
�5B
ǮB
��B
�B
�oB
�B
k�B
I�B
9XB
-B
&�B
!�B
�B
B	�sB	�
B	ǮB	�qB	�LB	�B	��B	�uB	�%B	s�B	hsB	gmB	ZB	K�B	=qB	$�B	\B	  B��B	  B�B�B�mB�ZB�NB�/B��BŢB�wB�qB�jB�^B�XB�FB�-B�B��B��B��B��B��B��B��B��B��B�uB�hB�hB�\B�VB�\B�hB�hB�\B�VB�bB�uB��BÖBƨBɺB�B�HB�NB�fB��B��B	B	B	B	%B	B	+B	B	B	B	B	%B	+B		7B	DB	PB	JB	VB	\B	{B	�B	�B	�B	$�B	-B	+B	%�B	(�B	,B	,B	33B	<jB	H�B	=qB	/B	/B	1'B	49B	49B	49B	:^B	>wB	?}B	G�B	G�B	O�B	P�B	Q�B	R�B	S�B	XB	XB	XB	YB	XB	YB	ZB	]/B	]/B	_;B	aHB	bNB	`BB	aHB	k�B	n�B	o�B	o�B	o�B	n�B	k�B	hsB	gmB	iyB	k�B	n�B	q�B	q�B	s�B	v�B	z�B	~�B	�B	�1B	�VB	�hB	�uB	�{B	�{B	�oB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�?B	�FB	�XB	�^B	�dB	�jB	�qB	�qB	�qB	��B	��B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�
B	�B	�B	�B	�#B	�;B	�BB	�;B	�5B	�/B	�/B	�;B	�BB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�BB	�5B	�5B	�5B	�)B	�)B	�5B	�;B	�5B	�)B	�B	�B	��B	��B	�
B	�)B	�5B	�;B	�BB	�HB	�HB	�BB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�ZB	�TB	�HB	�;B	�NB	�TB	�mB	�sB	�mB	�sB	�B	�yB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B
	7B

=B

=B

=B

=B
DB
JB
JB
JB
JB
PB
PB
PB
PB
JB
JB
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
�B
�B
�B
#�B
)�B
-B
6FB
@�B
D�B
F�B
J�B
P�B
W
B
[#B
`BB
cTB
ffB
l�B
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
�NB
�TB
�XB
�TB
�PB
�bB
�bB
�lB
�|B
̶B"�B��B�FB��B�!B2BM�Bb5Bn�Bw�Bw�B��B�1B��B�B�B��BB8B�B�B^B#�B:GBJ�B[B_%B_"BV�BN�BL�BN�BH�BD�BArB<TBP�BQ�BH�B<QB$�BbBB<BsB#�B+�B�BnB��B��B�1B�xBņB̰BĀB�B�CB��Bi[B\BU�BF�BH�BQ�BN�BBsB-�BKB�B�`B��B�2By�BkiBW�B?_B0B(B�B
��B
��B
�UB
�B
ǒB
�iB
��B
�SB
��B
kjB
I�B
9>B
,�B
&�B
!�B
yB
 �B	�\B	��B	ǘB	�ZB	�4B	��B	��B	�^B	�B	s�B	h^B	gWB	ZB	K�B	=_B	$�B	MB��B��B��B�B�B�[B�IB�<B�B��BŒB�gB�`B�ZB�MB�HB�6B�B�B��B��B��B��B��B��B�B�wB�pB�fB�WB�YB�NB�EB�NB�YB�ZB�MB�EB�RB�dB��BÃBƔBɨB��B�4B�<B�QB��B��B	�B	B	
B	B		B	B	B	�B	 �B	B	B	B		#B	/B	:B	2B	=B	EB	eB	�B	�B	oB	$�B	,�B	*�B	%�B	(�B	+�B	+�B	3B	<OB	H�B	=YB	/B	/B	1B	4!B	4 B	4 B	:EB	>^B	?eB	G�B	G�B	O�B	P�B	Q�B	R�B	S�B	W�B	W�B	W�B	X�B	W�B	X�B	ZB	]B	]B	_B	a/B	b1B	`#B	a,B	kjB	nB	o�B	o�B	o�B	nB	kiB	hYB	gPB	i_B	kkB	n}B	q�B	q�B	s�B	v�B	z�B	~�B	��B	�B	�8B	�MB	�YB	�^B	�_B	�NB	�8B	�GB	�cB	�oB	�oB	�dB	�nB	�nB	�oB	�nB	�tB	�{B	�{B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�%B	�:B	�@B	�FB	�MB	�UB	�RB	�RB	�cB	�kB	�xB	łB	ǐB	ȓB	ɝB	ʣB	˧B	κB	ϿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�B	�B	�B	�B	�B	�"B	�.B	�;B	�:B	�;B	�9B	�9B	�4B	�-B	�&B	�"B	�B	�B	�B	�B	�B	�B	�B	�B	�	B	��B	��B	��B	��B	��B	�
B	�B	�B	�!B	�'B	�'B	�!B	�)B	�4B	�:B	�:B	�8B	�9B	�;B	�>B	�:B	�;B	�5B	�'B	�B	�,B	�1B	�NB	�RB	�MB	�SB	�^B	�XB	�KB	�LB	�]B	�eB	�eB	�pB	�oB	�eB	�dB	�dB	�eB	�iB	�pB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
B
	B
B

B
B
B
	B
	B

B

B
	B

B

B

B

B
"B
'B
(B
(B
(B
.B
,B
/B
.B
)B
'B
<B
;B
:B
@B
@B
?B
AB
?B
AB
@B
AB
GB
EB
EB
FB
DB
DB
DB
CB
LB
LB
MB
LB
SB
QB
VB
TB
QG�O�B
iB
�B
#�B
)�B
,�B
6"B
@_B
DwB
F�B
J�B
P�B
V�B
Z�B
`B
c1B
f@B
lgB
pB
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451152016080714511520160807145115  AO  ARCAADJP                                                                    20150226221441    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221441  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221441  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145115  IP                  G�O�G�O�G�O�                