CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:15:22Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121041522  20190604095301  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @���R}5�1   @��݅OR�@9��t�j�c��vȴ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D3��D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�D��D�FfD��\D���D�qD�NfD�o\D�� D��D�P�D���D��fD� D�B=Dڄ�D�ؤD��D�?�D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Mp�@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B�8RB�k�B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\Cph�CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3�qD4qD4��D5�D5��D6=D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�=Dy��D��D�PRD��HD���D�]D�XRD�yHD���D�"�D�Z�D���D��RD��D�L)Dڎ�D��D��D�I�D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̅A�|�A�|�A�~�ÁÁA̋DA̍PȂhA̓uA̕�A̙�A̙�A̛�A̝�A̝�A̝�A̝�A̟�A̟�A̡�A̡�A̡�A̟�Ạ�Ḁ�Ạ�A̡�A̛�A̍PÃA�K�A�1'A�ȴA��yA��yA�ZA�x�A��A���A�ĜA���A���A��A���A�"�A��HA��wA��A�1A���A�jA���A��DA��RA�A�oA��A��FA�ffA��A��uA�bA�x�A��7A���A�%A�/A��FA�p�A�VA��A��7A���A�=qA�I�A��-A���A�?}A���A��A�"�A��/A�M�A��A��/A�bNA��;A��
A�hsA���A��HA�I�A�I�A�z�A��FA�%A�=qA�l�A���A�jA�7LA�-A�E�A�S�A�|�A��A�VA��9A���A�XA�jA�jA�ĜA��TA���A���A��\A�ȴA��PA�{A�`BA���A��PA��A33A|5?A{��Az  Aw�#As��Ar�DAq�Aq�As
=Ar�ApȴAo��Ao7LAn�jAm��Al��Ak��Ak`BAj�RAiAi"�Ah��Ag�;Ag`BAgoAeS�Ac�-Abr�Aa\)A`5?A^�A]�A]?}A\$�A[l�AZ=qAXjAW`BAU��AU/ATI�ASl�AR��ARZAO`BAN5?AM`BAK�AG��AE��ADĜABz�AA�AA33A@=qA?t�A>��A>^5A=p�A<�jA;`BA:��A:�!A9�TA8ZA7�-A6��A5K�A4bA3��A3p�A2M�A1oA01'A/�7A.�A. �A.�A-�
A-A,r�A,bA++A)�#A((�A'hsA&^5A%\)A$E�A#��A"��A"$�A!�-A!%A 5?A��At�Ax�A�Av�A$�AC�An�A`BA��AI�A�^A�/A=qA��AS�AVA�TAȴA{Ax�A��A��Ap�AƨAv�A?}A�\A��A	�AȴAQ�A�hA
=A^5AK�A��A`BA33A��AffA�A?}A �DA ff@��@�J@���@�{@���@���@��@�D@�\@��@��;@��@��@�?}@��@ꗍ@���@�/@�1@�+@��@�33@�h@�S�@݁@܃@�v�@؋D@�ƨ@֗�@�5?@�&�@Ӿw@�^5@�@���@ϝ�@Χ�@�O�@̣�@˾w@ʗ�@ɺ^@��/@Ǯ@���@��H@�=q@�I�@�dZ@�ȴ@�@�%@��@�~�@�^5@���@�r�@�;d@���@�=q@���@��;@�+@�M�@���@��`@��;@��@�M�@�$�@���@��/@��D@�  @��R@�?}@�1'@�1@��P@���@���@�A�@��m@���@��@��\@�@���@�hs@�&�@��@�1@�1@��u@�1'@���@�|�@�l�@�
=@�^5@�J@���@�1@�ƨ@��@�V@�5?@�$�@���@��@�  @���@�l�@�+@���@�=q@�@���@�b@�  @�1'@��w@�o@�$�@���@�bN@��@�;d@�o@��@��R@���@���@�`B@���@�9X@��@�;d@�ȴ@���@�n�@���@��/@��u@��@���@���@�S�@�o@��@��@��!@���@�$�@��T@��^@���@���@�J@�M�@�J@��T@���@��h@��j@�z�@��m@���@�C�@�
=@�@��@��H@��@�~�@�V@���@���@��@�hs@�O�@�G�@�?}@�%@��`@�Ĝ@���@��@�A�@�\)@�@��+@�=q@�{@��^@�G�@�%@��`@�Ĝ@���@�z�@�bN@�b@��@\)@
=@~�y@~ȴ@~��@~v�@~@}@}�@}�@|�j@|�D@|z�@|I�@{�m@{C�@z�@z��@z^5@x�@r��@m;@e��@]=�@UF@N�@HN�@C4�@:v�@5<6@0<�@+�@&3�@ ��@��@��@� @�<@�@U21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A̅A�|�A�|�A�~�ÁÁA̋DA̍PȂhA̓uA̕�A̙�A̙�A̛�A̝�A̝�A̝�A̝�A̟�A̟�A̡�A̡�A̡�A̟�Ạ�Ḁ�Ạ�A̡�A̛�A̍PÃA�K�A�1'A�ȴA��yA��yA�ZA�x�A��A���A�ĜA���A���A��A���A�"�A��HA��wA��A�1A���A�jA���A��DA��RA�A�oA��A��FA�ffA��A��uA�bA�x�A��7A���A�%A�/A��FA�p�A�VA��A��7A���A�=qA�I�A��-A���A�?}A���A��A�"�A��/A�M�A��A��/A�bNA��;A��
A�hsA���A��HA�I�A�I�A�z�A��FA�%A�=qA�l�A���A�jA�7LA�-A�E�A�S�A�|�A��A�VA��9A���A�XA�jA�jA�ĜA��TA���A���A��\A�ȴA��PA�{A�`BA���A��PA��A33A|5?A{��Az  Aw�#As��Ar�DAq�Aq�As
=Ar�ApȴAo��Ao7LAn�jAm��Al��Ak��Ak`BAj�RAiAi"�Ah��Ag�;Ag`BAgoAeS�Ac�-Abr�Aa\)A`5?A^�A]�A]?}A\$�A[l�AZ=qAXjAW`BAU��AU/ATI�ASl�AR��ARZAO`BAN5?AM`BAK�AG��AE��ADĜABz�AA�AA33A@=qA?t�A>��A>^5A=p�A<�jA;`BA:��A:�!A9�TA8ZA7�-A6��A5K�A4bA3��A3p�A2M�A1oA01'A/�7A.�A. �A.�A-�
A-A,r�A,bA++A)�#A((�A'hsA&^5A%\)A$E�A#��A"��A"$�A!�-A!%A 5?A��At�Ax�A�Av�A$�AC�An�A`BA��AI�A�^A�/A=qA��AS�AVA�TAȴA{Ax�A��A��Ap�AƨAv�A?}A�\A��A	�AȴAQ�A�hA
=A^5AK�A��A`BA33A��AffA�A?}A �DA ff@��@�J@���@�{@���@���@��@�D@�\@��@��;@��@��@�?}@��@ꗍ@���@�/@�1@�+@��@�33@�h@�S�@݁@܃@�v�@؋D@�ƨ@֗�@�5?@�&�@Ӿw@�^5@�@���@ϝ�@Χ�@�O�@̣�@˾w@ʗ�@ɺ^@��/@Ǯ@���@��H@�=q@�I�@�dZ@�ȴ@�@�%@��@�~�@�^5@���@�r�@�;d@���@�=q@���@��;@�+@�M�@���@��`@��;@��@�M�@�$�@���@��/@��D@�  @��R@�?}@�1'@�1@��P@���@���@�A�@��m@���@��@��\@�@���@�hs@�&�@��@�1@�1@��u@�1'@���@�|�@�l�@�
=@�^5@�J@���@�1@�ƨ@��@�V@�5?@�$�@���@��@�  @���@�l�@�+@���@�=q@�@���@�b@�  @�1'@��w@�o@�$�@���@�bN@��@�;d@�o@��@��R@���@���@�`B@���@�9X@��@�;d@�ȴ@���@�n�@���@��/@��u@��@���@���@�S�@�o@��@��@��!@���@�$�@��T@��^@���@���@�J@�M�@�J@��T@���@��h@��j@�z�@��m@���@�C�@�
=@�@��@��H@��@�~�@�V@���@���@��@�hs@�O�@�G�@�?}@�%@��`@�Ĝ@���@��@�A�@�\)@�@��+@�=q@�{@��^@�G�@�%@��`@�Ĝ@���@�z�@�bN@�b@��@\)@
=@~�y@~ȴ@~��@~v�@~@}@}�@}�@|�j@|�D@|z�@|I�@{�m@{C�@z�@z��G�O�@x�@r��@m;@e��@]=�@UF@N�@HN�@C4�@:v�@5<6@0<�@+�@&3�@ ��@��@��@� @�<@�@U21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�`B�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�`B�mB�mB�B
=B$�BJBcTB�Bm�BH�Be`B]/BD�B.B2-B7LB49B33BK�Bp�B�B{�B��B��B��B��B��B��B�B�;B�BB�#B�#B�TB�BB�)B��B��B��B��BǮB�wB�RB�B��B��B�\B�+B�B� B{�Bz�Bs�Bn�BgmBaHB]/BK�B<jB+B�B�B2-BG�B<jB0!B�BVB��B�NBĜB�?B��B�hB�B~�Bw�Bo�BffBYBG�B49B)�B7LBB
�ZB
�dB
��B
�PB
�%B
{�B
r�B
cTB
]/B
gmB
Q�B
^5B
I�B
.B
  B	��B	��B
�B
B�B
?}B
9XB
1'B
.B
)�B
#�B
�B
�B
�B
oB

=B
+B
B	��B	��B	�B	�`B	�B	��B	ƨB	�qB	�?B	�B	��B	��B	��B	��B	�bB	�+B	|�B	v�B	p�B	hsB	aHB	[#B	G�B	B�B	<jB	#�B��B�B�BB��B��B��B��B��B��B��B��B��B��BǮBŢB�wB�?B�!B��B��B��B��B��B�hB�VB�DB�DB�\B��B��B��B�{B�oB�VB�+B� Bv�Br�Bn�Bo�Bo�Bk�BgmBffBffBiyBk�Bk�BjBiyBhsBffBdZBbNB_;B_;B\)B[#BYBW
BW
BT�BQ�BN�BL�BK�BI�BH�BG�BE�BC�B?}B<jB:^B9XB7LB33B33B2-B0!B/B-B+B)�B)�B(�B'�B&�B$�B#�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBhBbB\BbBhBhBbBoBhBoBuBuBuB{BuBoBuBuBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B!�B$�B#�B#�B%�B(�B+B+B.B0!B1'B2-B2-B33B5?B7LB:^B:^B<jB>wB?}B@�BD�BE�BH�BN�BL�BL�BJ�BK�BM�BM�BN�BP�BQ�BQ�BS�BT�BS�BT�BXB]/B`BBaHBcTBffBffBgmBhsBiyBl�Bq�Bt�Bx�Bx�Bx�Bv�Bx�By�B}�B~�B�B�+B�7B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�FB�wB��BBĜB��B��B��B��B�B�#B�/B�BB�ZB�`B�yB�B�B�B�B��B��B��B	  B	B	B	B	%B	+B	
=B	JB	PB	VB	bB	hB	�B	�B	�B	�B	#�B	$�B	'�B	+B	+B	,B	/B	1'B	49B	6FB	9XB	;dB	C�B	D�B	G�B	J�B	K�B	P�B	R�B	T�B	XB	ZB	]/B	^5B	_;B	bNB	dZB	e`B	gmB	gmB	gmB	hsB	iyB	k�B	l�B	n�B	p�B	r�B	t�B	u�B	w�B	z�B	~�B	�B	�B	�B	��B	��B	ÖB	�SB	�|B

�B
_B
 �B
+QB
1[B
8�B
@iB
GEB
K�B
RTB
Y1B
`vB
e,B
k�B
p�B
ut1111111111111111111111111111111111114111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B�FB�OB�OB�LB�OB�OB�LB�KB�NB�OB�NB�KB�KB�NB�NB�NB�NB�NB�KB�NB�NB�NB�LB�LB�NB�NB�NB�NB�NB�NB�HB�TB�UB�qB
#B$�G�O�Bc=B��BmwG�O�BeBB]BD�B-�B2B75B4B3BK�Bp�B��B{�B��B��B��B��B��B��B��B�B�(B�
B�
B�9B�(B�BξBʦB̯B˪BǕB�WB�8B��B��B�{B�@B�B��B�B{�Bz�Bs�Bn{BgSBa*B]BK�B<NB*�B�BlB2BG�B<PB0B�B7B��B�0BĀB�!B��B�IB��B~�Bw�Bo~BfHBX�BG�B4B)�B72B �B
�=B
�DB
��B
�4B
�B
{�B
r�B
c4B
]B
gMB
Q�B
^B
I�B
-�B	��B	��B	��B
`B
BmB
?^B
96B
1B
-�B
)�B
#�B
�B
qB
nB
PB

B

B
�B	��B	��B	�B	�AB	��B	θB	ƈB	�PB	�B	��B	��B	��B	��B	�yB	�@B	�	B	|�B	v�B	p�B	hRB	a'B	[B	G�B	BnB	<IB	#�B��B�dB�!B��B̪BͲB��B��B��B��B��BθBʟBǊBŁB�TB�B��B��B��B�|B�jB�^B�DB�5B�!B�"B�8B�iB�xB�iB�YB�KB�4B�
B�Bv�Br�BnrBozBo|Bk`BgJBfCBfBBiWBk_BkbBj\BiTBhQBfABd8Bb*B_B_B\B[ BX�BV�BV�BT�BQ�BN�BL�BK�BI�BH�BG�BE}BCoB?XB<EB:7B94B7&B3B3B2B/�B.�B,�B*�B)�B)�B(�B'�B&�B$�B#�B#�B"�B!�B�B�B�B�BBxBuBrBnBuBsBkBhB_B\BTBUBMBHBCB<B8B<BBBCB8BJBDBHBOBPBOBTBLBIBNBOBMBPBMBTBRBYB\BfBeB_BqByB}BvB�B!�B$�B#�B#�B%�B(�B*�B*�B-�B/�B1 B2B2B3B5B7$B:7B:6B<@B>NB?TB@[BDvBEzBH�BN�BL�BL�BJ�BK�BM�BM�BN�BP�BQ�BQ�BS�BT�BS�BT�BW�B]	B`Ba Bc.Bf>Bf>BgDBhKBiQBlcBqBt�Bx�Bx�Bx�Bv�Bx�By�B}�B~�B��B�B�B�#B�YB�ZB�jB�rB�oB�vB��B�}B�vB�~B��B��B��B��B��B��B��B��B��B�B�B�B�PB�bB�fB�sBʗBϷB��B��B��B��B�	B�B�0B�8B�OB�`B�{B�B�B��B��B��B��B	�B	�B	�B	�B	B	
B	"B	&B	-B	9B	@B	WB	iB	wB	�B	#�B	$�B	'�B	*�B	*�B	+�B	.�B	0�B	4B	6B	9.B	;=B	ClB	DqB	G�B	J�B	K�B	P�B	R�B	T�B	W�B	Y�B	]B	^B	_B	b#B	d1B	e7B	gCB	gCB	gCB	hIB	iRB	kZB	lbB	nnB	p{B	r�B	t�B	u�B	w�B	z�B	~�B	��B	��G�O�B	��B	�vB	�lB	�)B	�PB

�B
5B
 �B
+(B
13B
8�B
@?B
GB
K�B
R,B
YB
`NB
eB
k�B
p�B
uM1111111111111111111111111111111111114111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953012019060409530120190604095301  AO  ARCAADJP                                                                    20181121041522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041522  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095301  IP                  G�O�G�O�G�O�                