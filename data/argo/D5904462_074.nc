CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-07T19:17:14Z AOML 3.0 creation; 2016-08-07T21:51:21Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150907191714  20160807145121  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               JA   AO  5287_9017_074                   2C  D   APEX                            6529                            072314                          846 @�m�0�Q1   @�m�d�Z@0r���m�d�� ě�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    JA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D��D�L�D�|�D��3D�	�D�<�D�s3D�� D�  D�6fD��fD��3D��D�FfDڙ�D��fD�� D�9�D�VfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB�#�B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B�#�B�#�B�qB�qB��B��B��B��B��B��C xRC��CxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt�zDy�GD�(�D�[�D���D��=D��D�K�D��=D��
D�
D�EpD��pD��=D��D�UpDڨ�D��pD��
D�H�D�epD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�M�A�O�A�Q�A�A�A��TA�-A��A蟾A�/A��/A�^A畁A�O�A��A�x�A��A���A�E�A�;dA�"�A��A�{A��A�;dA�K�A�33A�jA�O�A�33A���A�&�A�A�VAֲ-A���AԴ9A�7LA�9XAґhA�5?A�O�A�+A��A�A�C�A�C�A�|�A�^5A̓uA���A�v�A�(�Aț�A�9XA��AčPAÙ�AA��A���A��#A��HA��^A�C�A��TA��hA�7LA�ȴA�\)A��A���A��jA���A�ffA�A���A��+A�1'A�%A�hsA���A���A�
=A�
=A�v�A��
A��A�/A� �A��RA�-A�hsA��+A���A���A��DA�E�A�;dA���A�-A�(�A7LA~(�A{p�Aw�7At�\ArffAo�;Am+Aj��Ag33AdbNAbffA_"�A\1AZ^5AW��AS;dAQ�wANr�AK33AG��AE��AD��AD��AB�9AA��AAVA@(�A?|�A>�/A=�A<ĜA:�`A81A6�`A6^5A5�-A4I�A3VA21'A1�7A/A,�HA+A+VA*(�A)%A'��A&E�A%A$v�A"��A!t�A!A z�AVAA�A��AVA�wAz�A��AhsA�A �Al�An�A�mA��A�A�A�A�mA��A�\A
z�A
1A��A�!AƨAȴAbA�TAI�A=qAt�A�!A  A�A%A 9XA �@��w@��@�p�@���@��@�z�@���@��
@���@��R@�ff@��@���@��@�@��7@�O�@�&�@���@��#@�?}@��h@���@�/@�w@���@�
=@�@�v�@�b@�$�@�ȴ@�33@��;@�X@���@�p�@�A�@�w@�\)@�!@�K�@�@�|�@�ff@��#@�7L@��D@�1@�\@�5?@��T@�?}@웦@�  @�F@�@��@��@��/@�\)@�M�@�X@��/@�u@�I�@�l�@�dZ@�I�@�@�w@�;d@���@�l�@��@�t�@�A�@�ƨ@߶F@���@���@��m@��
@�ƨ@�|�@�o@�E�@�M�@���@�-@�&�@�Q�@ܓu@ܓu@܋D@ۅ@�5?@��@�b@��@��@�t�@��;@��m@��;@Ӿw@��H@�&�@Ѓ@�l�@�I�@д9@�1'@ϕ�@�
=@Η�@�ff@�~�@Ο�@�v�@�v�@�-@ͩ�@�x�@�X@��@��@̃@�bN@��@˶F@�S�@�+@��@ʗ�@�J@�V@Ȭ@ȋD@�A�@�b@���@�t�@�"�@ƸR@��T@�G�@��@��@�V@��@�(�@Å@�@�-@���@��@�ƨ@�n�@���@���@�I�@�1@���@���@��
@�|�@�33@�
=@��H@�~�@�=q@���@�p�@�%@�V@��@�A�@���@���@�n�@�{@��-@��h@�O�@�7L@�`B@�?}@��-@�x�@�hs@��h@�Ĝ@�dZ@���@��@��h@�x�@�&�@��@��u@�I�@�A�@���@��w@�l�@��\@�M�@�-@�$�@�{@�@��h@�hs@���@��u@�I�@��@��F@�t�@�33@�+@�t�@�K�@���@�~�@�v�@�v�@�ff@�=q@�J@��h@�hs@��7@�?}@���@�Z@��w@��@�ȴ@���@�v�@�ff@�V@��@��h@�p�@�O�@��/@���@�z�@�9X@�1@�dZ@���@���@�=q@���@�/@��`@�Ĝ@��u@��D@�z�@�z�@�r�@�bN@�bN@�Q�@�9X@�b@�ƨ@��P@�l�@�S�@��@�E�@�-@��@��7@�?}@���@���@��D@��j@�Ĝ@��@�t�@��+@�=q@���@��^@�O�@�&�@�Ĝ@���@�%@�x�@�t�@xĜ@nȴ@c�
@[o@Q&�@I��@B��@<j@41@,�/@'��@"�@�h@&�@��@^5@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�I�A�M�A�O�A�Q�A�A�A��TA�-A��A蟾A�/A��/A�^A畁A�O�A��A�x�A��A���A�E�A�;dA�"�A��A�{A��A�;dA�K�A�33A�jA�O�A�33A���A�&�A�A�VAֲ-A���AԴ9A�7LA�9XAґhA�5?A�O�A�+A��A�A�C�A�C�A�|�A�^5A̓uA���A�v�A�(�Aț�A�9XA��AčPAÙ�AA��A���A��#A��HA��^A�C�A��TA��hA�7LA�ȴA�\)A��A���A��jA���A�ffA�A���A��+A�1'A�%A�hsA���A���A�
=A�
=A�v�A��
A��A�/A� �A��RA�-A�hsA��+A���A���A��DA�E�A�;dA���A�-A�(�A7LA~(�A{p�Aw�7At�\ArffAo�;Am+Aj��Ag33AdbNAbffA_"�A\1AZ^5AW��AS;dAQ�wANr�AK33AG��AE��AD��AD��AB�9AA��AAVA@(�A?|�A>�/A=�A<ĜA:�`A81A6�`A6^5A5�-A4I�A3VA21'A1�7A/A,�HA+A+VA*(�A)%A'��A&E�A%A$v�A"��A!t�A!A z�AVAA�A��AVA�wAz�A��AhsA�A �Al�An�A�mA��A�A�A�A�mA��A�\A
z�A
1A��A�!AƨAȴAbA�TAI�A=qAt�A�!A  A�A%A 9XA �@��w@��@�p�@���@��@�z�@���@��
@���@��R@�ff@��@���@��@�@��7@�O�@�&�@���@��#@�?}@��h@���@�/@�w@���@�
=@�@�v�@�b@�$�@�ȴ@�33@��;@�X@���@�p�@�A�@�w@�\)@�!@�K�@�@�|�@�ff@��#@�7L@��D@�1@�\@�5?@��T@�?}@웦@�  @�F@�@��@��@��/@�\)@�M�@�X@��/@�u@�I�@�l�@�dZ@�I�@�@�w@�;d@���@�l�@��@�t�@�A�@�ƨ@߶F@���@���@��m@��
@�ƨ@�|�@�o@�E�@�M�@���@�-@�&�@�Q�@ܓu@ܓu@܋D@ۅ@�5?@��@�b@��@��@�t�@��;@��m@��;@Ӿw@��H@�&�@Ѓ@�l�@�I�@д9@�1'@ϕ�@�
=@Η�@�ff@�~�@Ο�@�v�@�v�@�-@ͩ�@�x�@�X@��@��@̃@�bN@��@˶F@�S�@�+@��@ʗ�@�J@�V@Ȭ@ȋD@�A�@�b@���@�t�@�"�@ƸR@��T@�G�@��@��@�V@��@�(�@Å@�@�-@���@��@�ƨ@�n�@���@���@�I�@�1@���@���@��
@�|�@�33@�
=@��H@�~�@�=q@���@�p�@�%@�V@��@�A�@���@���@�n�@�{@��-@��h@�O�@�7L@�`B@�?}@��-@�x�@�hs@��h@�Ĝ@�dZ@���@��@��h@�x�@�&�@��@��u@�I�@�A�@���@��w@�l�@��\@�M�@�-@�$�@�{@�@��h@�hs@���@��u@�I�@��@��F@�t�@�33@�+@�t�@�K�@���@�~�@�v�@�v�@�ff@�=q@�J@��h@�hs@��7@�?}@���@�Z@��w@��@�ȴ@���@�v�@�ff@�V@��@��h@�p�@�O�@��/@���@�z�@�9X@�1@�dZ@���@���@�=q@���@�/@��`@�Ĝ@��u@��D@�z�@�z�@�r�@�bN@�bN@�Q�@�9X@�b@�ƨ@��P@�l�@�S�@��@�E�@�-@��@��7@�?}@���@���@��D@��j@�Ĝ@��@�t�@��+@�=q@���@��^@�O�@�&�G�O�@���@�%@�x�@�t�@xĜ@nȴ@c�
@[o@Q&�@I��@B��@<j@41@,�/@'��@"�@�h@&�@��@^5@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
7LB
7LB
7LB
7LB
6FB
5?B
/B
uB
hB
�B
�B
�B
!�B
!�B
#�B
%�B
�B
�B
�B
�B
�B
{B
hB
\B
JB
1B
B
B	��B	��B	��B
B
PB
,B
;dB
K�B
T�B
ZB
�!B
�
B
�ZB
��BE�BJ�BN�BhsB�7B��B��B�B�jB�mB
=B�B�B6FBdZBq�B�7B�PB�\B��B��B��B��B�=B}�By�Bv�B�B�DB�%BgmBbNB[#BK�B5?B�B{B5?BC�BA�B?}B9XB(�B8RB/B��B�B��B��B�hB� B_;BB
��B
�?B
��B
�VB
�+B
y�B
^5B
B�B
8RB
 �B	��B	�BB	��B	�wB	�B	��B	�oB	�B	y�B	jB	bNB	ZB	K�B	6FB	&�B	�B	JB	B	B	  B	B	B	B��B��B��B�B�B�yB�BB�B��B��B��B��B��BȴBƨBÖBÖBĜBĜBĜBĜB��B�)B�)B�/B�ZB�sB�sB�sB�sB�ZB�fB�B�B��B��B��B	B	B	%B	1B		7B		7B	B��B�B�sB�)B�B��B��BȴBƨBǮBƨBɺB��B�#B�)B�#B�/B�)B�HB�;B�/B�#B�B�B��B�B�`B�fB�mB�B��B��B��B��B��B��B�B��B��B��B	%B	hB	bB	�B	�B	�B	�B	�B	bB	PB	uB	$�B	>wB	F�B	R�B	[#B	jB	p�B	r�B	p�B	o�B	n�B	p�B	z�B	}�B	�1B	�=B	�=B	�DB	�VB	�PB	�oB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�FB	�LB	�LB	�RB	�^B	�dB	�dB	�dB	�dB	�qB	�wB	ÖB	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	ĜB	��B	��B	�
B	�
B	��B	��B	��B	��B	�B	�)B	�#B	�B	�B	�B	�B	�5B	�HB	�HB	�HB	�BB	�;B	�5B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�ZB	�HB	�5B	�5B	�/B	�#B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�B	�B	�B	�B	�#B	�/B	�BB	�NB	�TB	�`B	�fB	�yB	�yB	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B
PB
VB
VB
PB

=B

=B

=B
DB
DB
DB
{B
uB
uB
�B
�B
,B
2-B
:^B
@�B
G�B
L�B
P�B
VB
\)B
bNB
iyB
l�B
p�B
s�B
v�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
7/B
71B
71B
7/B
6+B
5"B
.�B
^B
LB
eB
�B
�B
!�B
!�B
#�B
%�B
�B
{B
pB
mB
mB
`B
KB
CB
/B
B
B
�B	��B	��B	��B
 �B
5B
+�B
;GB
K�B
T�B
Y�B
�B
��B
�7B
��BE|BJ�BN�BhKB�B��B��B��B�DB�IB
BiB�B6!Bd1Bq�B�B�*B�2B�YB�vB��B�fB�B}�By�Bv�B��B�B��BgCBb$BZ�BK�B5BjBRB5BCnBA_B?SB9/B(�B8$B.�B��B��B��B�vB�:B�B_B�B
��B
�B
�zB
�*B
� B
y�B
^	B
BeB
8'B
 �B	��B	�B	пB	�QB	��B	��B	�IB	��B	y�B	j\B	b)B	Y�B	K�B	6!B	&�B	^B	'B	�B	�B��B	 �B	 �B	 �B��B��B��B�B�sB�WB�!B��B��B��BϾB̪BʞBȔBƆB�rB�tB�{B�zB�zB�{B��B�B�B�B�7B�MB�OB�MB�PB�5B�AB�aB�B��B��B��B	 �B	�B	�B	
B		B		B	�B��B�|B�LB�B��BαB̧BȐBƀBǅB�BɒBͭB��B�B��B�B��B�B�B�B��B��B��B��B��B�5B�=B�DB�B��B��B��B��B��B��B�B��B��B��B	�B	<B	7B	]B	�B	�B	~B	dB	8B	$B	HB	$�B	>HB	FxB	R�B	Z�B	jPB	puB	r�B	puB	opB	nhB	puB	z�B	}�B	�B	�B	�B	�B	�'B	� B	�BB	�KB	�CB	�QB	�]B	�oB	�pB	��B	�qB	��B	��B	�vB	�\B	�XB	�WB	�UB	�VB	�NB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�.B	�2B	�3B	�3B	�4B	�>B	�FB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	̘B	�iB	ΧB	��B	��B	��B	ҿB	͞B	ϬB	͞B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�%B	�&B	�%B	�'B	�,B	�.B	�,B	�-B	�,B	�,B	�3B	�1B	�2B	�0B	�1B	�3B	�2B	�.B	�*B	�'B	�B	�B	�B	��B	��B	��B	ѶB	ϬB	̛B	̗B	͡B	ΦB	ΦB	ϪB	ЯB	бB	ѸB	ѷB	ѹB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�+B	�0B	�EB	�CB	�KB	�cB	�]B	�QB	�DB	�?B	�>B	�>B	�?B	�=B	�EB	�DB	�EB	�FB	�IB	�JB	�WB	�VB	�WB	�NB	�PB	�HB	�RB	�OB	�QB	�RB	�PB	�PB	�QB	�NB	�WB	�qB	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B
B
"B
B
B

B

B

B
B
B
G�O�B
>B
AB
uB
|B
+�B
1�B
:%B
@HB
GvB
L�B
P�B
U�B
[�B
bB
i>B
lUB
pkB
s}B
v�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451212016080714512120160807145121  AO  ARCAADJP                                                                    20150907191714    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150907191714  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150907191714  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145121  IP                  G�O�G�O�G�O�                