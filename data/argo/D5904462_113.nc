CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-31T19:17:57Z AOML 3.0 creation; 2016-08-07T21:51:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160331191757  20160807145128  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               qA   AO  5287_9017_113                   2C  D   APEX                            6529                            072314                          846 @סU�z1   @ס�s�b@-�fffff�d���O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    qA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B���B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dyl�D�3D�@ D��3D��3D� D�9�D�vfD��fD��D�@ D���D�� D��fD�C3D�p D�� D� D�0 D�3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBqz�By�HB��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��qB��B��B��B��B��B��B��B��B�W
B�qB��B��B�qB��B��qB��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2^�C4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D�D�DD�DD�zDD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7�D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�zDNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDtqGDy��D�"=D�O
D��=D��=D�
D�H�D��pD��pD��D�O
D���D��
D�pD�R=D�
D��
D�
D�?
D�=D��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aд9AжFAмjAмjAЁA�
=A��/A�+A̩�A�n�A�A�A� �A��`A���A˺^A˧�AˍPA�x�A�l�A�\)A�I�A�7LA�/A� �A��A�VA�1A���A��yA��A���Aʴ9A�t�A�7LA��AɋDA�E�A�bA��;Aȝ�A�
=AǅAƓuA�1A�E�A�A���A��/A�Q�A�r�A���A��^A��A���A���A��A�~�A�S�A�\)A��A�E�A�p�A��jA��A�9XA�XA���A� �A�l�A���A�A�K�A���A���A��A�1A�5?A�$�A�v�A�  A���A���A���A�jA��A�A��-A�l�A��\A�JA�r�A�
=A�?}A�r�A|��AwAsƨAq��Ao�wAoAnȴAn�RAnjAn9XAm��Al��Aj5?AeoAb��Aa7LA`1A^1A\��A[O�AZA�AW�
AUhsAT�AO�7AI��AE+AB�`A@ȴA>��A=C�A;C�A9��A8��A7?}A6I�A5��A4��A1�hA-O�A+O�A*�RA*��A)��A'\)A&9XA$Q�A"�!A!33A �Al�A��A=qA{A��A��A�FA�RAoAC�AhsA�A�A��A�+A�^A
��A
�jA
bNA	��A	S�A��AA�RA��A�yAffA�A�mA�^A��Ap�A?}A�A�`AĜA��A�uA^5AO�A (�@���@���@�hs@��-@���@�5?@��@�hs@�`B@�X@��@���@�5?@���@�w@�~�@�(�@�\)@�\@��#@�X@���@�bN@�9X@� �@��;@�+@�J@�`B@�bN@�C�@���@�F@�33@�5?@��@��@���@ߍP@ޟ�@���@��@�t�@��;@��
@��H@٩�@��`@ؓu@�9X@��;@�K�@��@�n�@ղ-@�X@�V@���@҇+@�Q�@��;@�C�@�~�@��@ͺ^@͉7@�O�@���@˕�@�;d@��y@�hs@Ȭ@�z�@�  @�1@Ǿw@�o@���@���@Ɵ�@�{@�J@��T@�`B@��/@å�@��@�@�$�@��-@�`B@�7L@���@�bN@�(�@��m@��@�l�@�K�@��!@�n�@��@�hs@��j@��@�A�@��@��
@���@�;d@��@��@���@�-@���@��7@�7L@��j@�Q�@���@���@�ff@�J@���@�x�@�?}@�Ĝ@��u@�Z@�  @��@�\)@�\)@�\)@��y@��@��@��#@��^@��-@���@�?}@��/@�bN@��R@�V@�J@��-@�x�@�G�@�`B@��7@��7@��@���@��h@�x�@�O�@�&�@��9@�Ĝ@��@�A�@� �@��@��;@�33@�5?@��T@���@���@�t�@�C�@�+@�
=@���@�~�@��@�J@��T@�Ĝ@���@�33@��@�n�@�=q@��@��-@���@��h@���@�Q�@�(�@���@�;d@�+@��@��!@���@�~�@�n�@�^5@�^5@�V@�=q@�-@�{@��^@�?}@�Ĝ@�z�@���@��P@�@��R@���@��+@��+@��+@�~�@�ff@�M�@�-@�@��#@��^@���@�hs@�/@���@��/@�Ĝ@���@��D@�I�@���@�|�@�+@�o@��@��R@���@�n�@�M�@�J@��@��9@���@�t�@�33@��@��H@���@�n�@�-@�J@��^@�hs@�?}@�%@���@�Z@��@�ƨ@�|�@�C�@��@��@���@�E�@���@��7@�/@��`@��9@��@�1'@��;@��w@���@��P@�S�@�o@��R@�V@�-@��@���@�hs@��@�z�@�  @��F@�|�@�C�@��@���@�^5@�M�@�-@�{@���@� �@|��@n��@c��@Z�\@R=q@I�^@B�@9�#@3S�@.�y@)7L@%?}@   @�j@�7@�@�\@K�@��@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aд9AжFAмjAмjAЁA�
=A��/A�+A̩�A�n�A�A�A� �A��`A���A˺^A˧�AˍPA�x�A�l�A�\)A�I�A�7LA�/A� �A��A�VA�1A���A��yA��A���Aʴ9A�t�A�7LA��AɋDA�E�A�bA��;Aȝ�A�
=AǅAƓuA�1A�E�A�A���A��/A�Q�A�r�A���A��^A��A���A���A��A�~�A�S�A�\)A��A�E�A�p�A��jA��A�9XA�XA���A� �A�l�A���A�A�K�A���A���A��A�1A�5?A�$�A�v�A�  A���A���A���A�jA��A�A��-A�l�A��\A�JA�r�A�
=A�?}A�r�A|��AwAsƨAq��Ao�wAoAnȴAn�RAnjAn9XAm��Al��Aj5?AeoAb��Aa7LA`1A^1A\��A[O�AZA�AW�
AUhsAT�AO�7AI��AE+AB�`A@ȴA>��A=C�A;C�A9��A8��A7?}A6I�A5��A4��A1�hA-O�A+O�A*�RA*��A)��A'\)A&9XA$Q�A"�!A!33A �Al�A��A=qA{A��A��A�FA�RAoAC�AhsA�A�A��A�+A�^A
��A
�jA
bNA	��A	S�A��AA�RA��A�yAffA�A�mA�^A��Ap�A?}A�A�`AĜA��A�uA^5AO�A (�@���@���@�hs@��-@���@�5?@��@�hs@�`B@�X@��@���@�5?@���@�w@�~�@�(�@�\)@�\@��#@�X@���@�bN@�9X@� �@��;@�+@�J@�`B@�bN@�C�@���@�F@�33@�5?@��@��@���@ߍP@ޟ�@���@��@�t�@��;@��
@��H@٩�@��`@ؓu@�9X@��;@�K�@��@�n�@ղ-@�X@�V@���@҇+@�Q�@��;@�C�@�~�@��@ͺ^@͉7@�O�@���@˕�@�;d@��y@�hs@Ȭ@�z�@�  @�1@Ǿw@�o@���@���@Ɵ�@�{@�J@��T@�`B@��/@å�@��@�@�$�@��-@�`B@�7L@���@�bN@�(�@��m@��@�l�@�K�@��!@�n�@��@�hs@��j@��@�A�@��@��
@���@�;d@��@��@���@�-@���@��7@�7L@��j@�Q�@���@���@�ff@�J@���@�x�@�?}@�Ĝ@��u@�Z@�  @��@�\)@�\)@�\)@��y@��@��@��#@��^@��-@���@�?}@��/@�bN@��R@�V@�J@��-@�x�@�G�@�`B@��7@��7@��@���@��h@�x�@�O�@�&�@��9@�Ĝ@��@�A�@� �@��@��;@�33@�5?@��T@���@���@�t�@�C�@�+@�
=@���@�~�@��@�J@��T@�Ĝ@���@�33@��@�n�@�=q@��@��-@���@��h@���@�Q�@�(�@���@�;d@�+@��@��!@���@�~�@�n�@�^5@�^5@�V@�=q@�-@�{@��^@�?}@�Ĝ@�z�@���@��P@�@��R@���@��+@��+@��+@�~�@�ff@�M�@�-@�@��#@��^@���@�hs@�/@���@��/@�Ĝ@���@��D@�I�@���@�|�@�+@�o@��@��R@���@�n�@�M�@�J@��@��9@���@�t�@�33@��@��H@���@�n�@�-@�J@��^@�hs@�?}@�%@���@�Z@��@�ƨ@�|�@�C�@��@��@���@�E�@���@��7@�/@��`@��9@��@�1'@��;@��w@���@��P@�S�@�o@��R@�V@�-@��@���@�hs@��@�z�@�  @��F@�|�@�C�@��@���@�^5@�M�@�-@�{G�O�@� �@|��@n��@c��@Z�\@R=q@I�^@B�@9�#@3S�@.�y@)7L@%?}@   @�j@�7@�@�\@K�@��@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B%B�B,B?}BB�BP�BcTB}�B��B�LB�;B�B5?B:^B8RB8RBB�BR�BbNBt�B}�B�B�%B�bB�B�1B�=By�Bu�B�B�B~�Bw�Br�Bs�Bm�BT�B9XB �B��B��BÖB�?B��B�JBu�Bs�Bo�Bm�B�+B�uB�oB�hB�=Bv�BJ�BB
��B
��B
�{B
u�B
N�B
P�B
L�B
@�B
49B
(�B
"�B
!�B
!�B
�B
�B
�B
bB	��B	�B	ȴB	�wB	�9B	��B	��B	�{B	�JB	|�B	l�B	`BB	D�B	 �B		7B	B��B�B�B�ZB�)B�B��B��BƨB��B�jB��BĜBŢBŢB��B�dB�jB��BBĜBƨBǮBǮBƨBƨBƨBƨBŢBÖB��B�jB�FB�FB�LB�LB�LB�FB�?B�?B�3B�9B�9B�?B�FB�XB�wBBƨBȴBɺB��B��B��B��B��B��B��B��B�B�
B�BB��B��B��B	
=B	�B	�B	�B	�B	 �B	 �B	!�B	!�B	 �B	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	+B	/B	0!B	1'B	2-B	6FB	=qB	B�B	B�B	A�B	B�B	D�B	D�B	F�B	G�B	G�B	G�B	J�B	L�B	[#B	`BB	e`B	iyB	jB	n�B	s�B	w�B	y�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�7B	�\B	�\B	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�LB	�RB	�XB	�dB	�qB	�wB	�}B	��B	��B	��B	ÖB	ÖB	B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�BB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
%B
%B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
-B
<jB
;dB
B�B
J�B
N�B
R�B
ZB
`BB
bNB
ffB
iyB
n�B
q�B
t�B
x�B
z�B
~�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�rB
�qB
�rB
�qB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��BBbB+�B?ZBBjBP�Bc,B}�B��B�&B�B�B5B:8B8+B8+BBiBR�Bb%Bt�B}�B��B��B�<B��B�B�By�Bu�B��B��B~�Bw�Br�Bs�BmgBT�B9,B �B��BϯB�hB�B��B�Bu�Bs�BorBmcB��B�IB�CB�:B�Bv�BJ�B �B
ϲB
��B
�QB
u�B
N�B
P�B
L�B
@WB
4B
(�B
"�B
!�B
!�B
�B
�B
pB
:B	��B	��B	ȍB	�OB	�B	��B	��B	�UB	�#B	|�B	ldB	`B	DwB	 �B		B	 �B��B�B�gB�7B�B��B��B̬BƅB�aB�HB�gB�yB�~BŀB�fB�@B�EB�aB�nB�xBƁBǇBǈBƁBƂBƂBƀB�}B�rB�[B�DB�!B�!B�'B�&B�&B�#B�B�B�B�B�B�B�"B�0B�PB�gB�BȍBɔBʚB̤BͭBϸBоB��B��B��B��B��B�B��B��B��B	
B	iB	{B	zB	�B	 �B	 �B	!�B	!�B	 �B	zB	pB	oB	eB	{B	�B	 �B	"�B	&�B	*�B	.�B	/�B	0�B	2 B	6B	=CB	BbB	B`B	AZB	BbB	DqB	DnB	FzB	G�B	G�B	G�B	J�B	L�B	Z�B	`B	e/B	iHB	jPB	niB	s�B	w�B	y�B	z�B	{�B	|�B	}�B	�B	��B	��B	��B	��B	�B	�)B	�(B	�7B	�=B	�JB	�HB	�LB	�OB	�PB	�TB	�VB	�VB	�jB	�tB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�1B	�?B	�EB	�HB	�PB	�PB	�UB	�bB	�cB	�ZB	�`B	�gB	�gB	�iB	�qB	�nB	�kB	�tB	�zB	�~B	ɅB	ϩB	ѵB	ѷB	ѶB	ѷB	ѶB	ѸB	аB	бB	бB	бB	бB	ѷB	ѵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�=B	�IB	�GB	�JB	�JB	�ZB	�oB	�jB	�hB	�pB	�hB	�oB	�tB	�{B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B

B

B

B

B

B

B

B

B

B

B
B
B
B
B
B
B
B
B
B
B
 B
B
%B
'B
%B
#B
'B
$B
$B
$B
%B
,B
*B
1B
.B
/B
7B
6B
7B
6B
3B
8B
>B
<B
;B
CB
DB
BB
CB
JB
IB
IB
JB
IB
OB
MB
TB
UB
[B
ZB
\B
^B
`B
bB
aB
cB
`B
hB
hB
iB
hB
mB
oB
kB
mB
tB
sB
uB
uB
tB
sB
uB
zB
yB
|B
�G�O�B
#�B
,�B
<1B
;+B
BVB
J�B
N�B
R�B
Y�B
`B
bB
f,B
i?B
n_B
qsB
t�B
x�B
z�B
~�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451282016080714512820160807145128  AO  ARCAADJP                                                                    20160331191757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160331191757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160331191757  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145128  IP                  G�O�G�O�G�O�                