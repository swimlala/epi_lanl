CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-10T09:16:40Z AOML 3.0 creation; 2016-08-07T21:51:18Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150610091640  20160807145118  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               9A   AO  5287_9017_057                   2C  D   APEX                            6529                            072314                          846 @�WB����1   @�WC8�@/e`A�7L�d�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    9A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD�fD�I�D�|�D�� D��D�FfD���D�� D� D�9�D��3D�ɚD���D�<�Dڀ D�ɚD��D�6fD� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=A�A'�AG�Ag�A�A�A�A�A���A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B�#�B��qB��B��B��B��B��B��B��BܽqB��B��B��B��B��B��B��B�#�C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuzDy�zD�%pD�X�D���D��
D��D�UpD���D��
D�
D�H�D��=D�ؤD��D�K�Dڏ
D�ؤD�(�D�EpD�
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA�bNA�`BA�9XA�&�A�VA���AۅA�"�A���Aڴ9A�&�AٶFAٙ�A�~�A�z�A�l�A�`BA�"�A���A��A�/A���A��
A֩�A�z�A�S�A�I�A�O�A�O�A�M�A�
=AլAթ�Aթ�AՕ�A�p�A�E�A�-A��HAԮA�~�A�C�A�~�A��Aϩ�A��A͡�A���A˅A�z�A�bA�v�A�l�A�~�A�&�A�Q�A��+A���A�-A�"�A�  A�=qA�?}A��A���A���A��+A���A��jA���A��^A���A���A��A��
A��wA�?}A���A��PA��TA��DA���A�bA���A�jA��
A�M�A�oA��A��A�mAz�Av~�As%An��Ak�AiG�Af�Ad�Abz�A]�AX9XAV1AS�7AP�AM��AIADbABv�AA��AA
=A@-A?;dA=�mA<�A;��A:�uA9ƨA8�DA7C�A6�RA6-A5O�A4z�A3x�A2~�A0�`A.E�A,�A)C�A&��A#��A Q�A��A;dA��A�A��A�wA1A7LA��A�FAdZA�`AI�A�A�-AQ�A9XA��A;dA��Ar�A�A�!A��A�A�At�A
�A	��A	l�A�!A�;A��AdZA�\A|�AjA�A�A�9A�wA�AA ��@��;@�^5@�dZ@�$�@�G�@�bN@��P@��@��#@�hs@�x�@�&�@��/@��@���@�@�A�@@�^5@�@�j@�ƨ@�|�@�"�@�~�@�I�@�F@�~�@�-@䛦@� �@�dZ@���@�Q�@�;d@�
=@�@��@�@�j@ە�@ڰ!@��T@ٲ-@ى7@�O�@�G�@��`@���@؋D@�z�@�b@�C�@֧�@���@��`@�j@��;@ӥ�@��@�%@�Ĝ@���@��@Χ�@�^5@���@�&�@�Z@ˍP@ʗ�@��@���@���@�l�@�K�@�+@�v�@�?}@Ĵ9@�1'@�ƨ@��
@��;@���@��y@�{@���@�G�@��/@� �@��P@��@���@�Ĝ@�bN@�Z@�?}@�?}@�(�@��
@�1@���@�1'@���@�dZ@�@��@���@�p�@�V@�?}@��-@�/@��j@�bN@���@���@�(�@�9X@�dZ@�
=@�ȴ@���@��-@�1'@��@�b@��m@�t�@�"�@���@��@�ȴ@���@�~�@�=q@��#@��7@�`B@�X@��@���@��`@���@���@��w@���@�t�@�S�@�+@��@�ȴ@��!@�M�@��@��-@�p�@�O�@�O�@�O�@�/@�V@��9@� �@��@���@�C�@���@�ȴ@��!@�E�@�x�@�/@���@��D@�Q�@�1@���@�\)@��y@��\@�^5@�=q@�{@���@��^@��h@�x�@�X@�V@�bN@��P@�+@��y@���@���@�n�@�M�@�E�@�E�@��@��h@�p�@�7L@���@��u@�(�@�  @�ƨ@�K�@�+@�o@��y@�ȴ@��!@��\@�v�@�n�@�E�@�5?@�$�@��T@�p�@�/@���@��`@��j@��D@�j@�j@�Q�@� �@�  @���@��
@�ƨ@��w@��@�K�@�o@�@��@�ȴ@���@��!@�^5@�E�@�@��h@�X@��`@�A�@���@��w@��@���@�~�@�ff@�V@��@���@���@��j@��@��u@�  @�t�@�+@���@���@�n�@�=q@���@���@�&�@��@�%@�j@��@�ƨ@�t�@�@��y@��R@�v�@��@��-@�/@��/@�Ĝ@�z�@�1@���@���@�;d@�o@�ȴ@���@�v�@�^5@�E�@�E�@�$�@���@��-@�hs@�&�@�V@���@��@��7@�bN@x�9@o|�@f{@\Z@R-@H�`@>@4��@/+@)�@&v�@   @"�@$�@J@$�@S�@�@�y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ffA�bNA�`BA�9XA�&�A�VA���AۅA�"�A���Aڴ9A�&�AٶFAٙ�A�~�A�z�A�l�A�`BA�"�A���A��A�/A���A��
A֩�A�z�A�S�A�I�A�O�A�O�A�M�A�
=AլAթ�Aթ�AՕ�A�p�A�E�A�-A��HAԮA�~�A�C�A�~�A��Aϩ�A��A͡�A���A˅A�z�A�bA�v�A�l�A�~�A�&�A�Q�A��+A���A�-A�"�A�  A�=qA�?}A��A���A���A��+A���A��jA���A��^A���A���A��A��
A��wA�?}A���A��PA��TA��DA���A�bA���A�jA��
A�M�A�oA��A��A�mAz�Av~�As%An��Ak�AiG�Af�Ad�Abz�A]�AX9XAV1AS�7AP�AM��AIADbABv�AA��AA
=A@-A?;dA=�mA<�A;��A:�uA9ƨA8�DA7C�A6�RA6-A5O�A4z�A3x�A2~�A0�`A.E�A,�A)C�A&��A#��A Q�A��A;dA��A�A��A�wA1A7LA��A�FAdZA�`AI�A�A�-AQ�A9XA��A;dA��Ar�A�A�!A��A�A�At�A
�A	��A	l�A�!A�;A��AdZA�\A|�AjA�A�A�9A�wA�AA ��@��;@�^5@�dZ@�$�@�G�@�bN@��P@��@��#@�hs@�x�@�&�@��/@��@���@�@�A�@@�^5@�@�j@�ƨ@�|�@�"�@�~�@�I�@�F@�~�@�-@䛦@� �@�dZ@���@�Q�@�;d@�
=@�@��@�@�j@ە�@ڰ!@��T@ٲ-@ى7@�O�@�G�@��`@���@؋D@�z�@�b@�C�@֧�@���@��`@�j@��;@ӥ�@��@�%@�Ĝ@���@��@Χ�@�^5@���@�&�@�Z@ˍP@ʗ�@��@���@���@�l�@�K�@�+@�v�@�?}@Ĵ9@�1'@�ƨ@��
@��;@���@��y@�{@���@�G�@��/@� �@��P@��@���@�Ĝ@�bN@�Z@�?}@�?}@�(�@��
@�1@���@�1'@���@�dZ@�@��@���@�p�@�V@�?}@��-@�/@��j@�bN@���@���@�(�@�9X@�dZ@�
=@�ȴ@���@��-@�1'@��@�b@��m@�t�@�"�@���@��@�ȴ@���@�~�@�=q@��#@��7@�`B@�X@��@���@��`@���@���@��w@���@�t�@�S�@�+@��@�ȴ@��!@�M�@��@��-@�p�@�O�@�O�@�O�@�/@�V@��9@� �@��@���@�C�@���@�ȴ@��!@�E�@�x�@�/@���@��D@�Q�@�1@���@�\)@��y@��\@�^5@�=q@�{@���@��^@��h@�x�@�X@�V@�bN@��P@�+@��y@���@���@�n�@�M�@�E�@�E�@��@��h@�p�@�7L@���@��u@�(�@�  @�ƨ@�K�@�+@�o@��y@�ȴ@��!@��\@�v�@�n�@�E�@�5?@�$�@��T@�p�@�/@���@��`@��j@��D@�j@�j@�Q�@� �@�  @���@��
@�ƨ@��w@��@�K�@�o@�@��@�ȴ@���@��!@�^5@�E�@�@��h@�X@��`@�A�@���@��w@��@���@�~�@�ff@�V@��@���@���@��j@��@��u@�  @�t�@�+@���@���@�n�@�=q@���@���@�&�@��@�%@�j@��@�ƨ@�t�@�@��y@��R@�v�@��@��-@�/@��/@�Ĝ@�z�@�1@���@���@�;d@�o@�ȴ@���@�v�@�^5@�E�@�E�@�$�@���@��-@�hs@�&�@�V@���G�O�@��7@�bN@x�9@o|�@f{@\Z@R-@H�`@>@4��@/+@)�@&v�@   @"�@$�@J@$�@S�@�@�y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�yB	�yB	�sB	�sB	�B	��B
+B
hB
I�B
k�B
jB
k�B
l�B
o�B
u�B
|�B
|�B
o�B
cTB
bNB
bNB
e`B
k�B
p�B
t�B
y�B
{�B
~�B
{�B
�PB
�\B
�\B
�oB
��B
��B
��B
��B
��B
ÖB
�B
��B+B!�BE�BQ�BdZB~�B�\B��B��B�TB�`B�ZB�NB�TB�mB�fB�B�yB�NB�B��B�`B��B��BȴB�B��B��B��B��B��B�BffBN�B6FB�B
�ZB
��B
��B
��B
��B
��B
��B
�=B
x�B
ffB
E�B
VB	�ZB	B	�B	�oB	y�B	ffB	T�B	L�B	D�B	5?B	$�B	�B	�B	PB	  B�B�yB�`B�ZB�HB�;B�/B�)B�B�B�B�
B�
B�
B�B�B�B�B�B�
B�
B�
B��B��B��B��B��B��B�B�
B�B�B�B�#B�/B�/B�BB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B�B�sB�5B�
B�#B�`B�B�B�B�B�B��B��B��B��B��B��B�B�B��B		7B	oB	uB	{B	�B	�B	�B	 �B	#�B	%�B	&�B	$�B	#�B	!�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	&�B	)�B	+B	,B	.B	1'B	6FB	5?B	?}B	I�B	K�B	L�B	N�B	M�B	R�B	ZB	e`B	iyB	iyB	jB	k�B	l�B	m�B	o�B	p�B	s�B	u�B	v�B	y�B	y�B	z�B	~�B	�B	�B	�B	�B	�B	� B	� B	�B	� B	|�B	|�B	|�B	|�B	{�B	|�B	~�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�=B	�=B	�=B	�7B	�7B	�+B	�B	�B	�1B	�VB	�bB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�!B	�-B	�9B	�3B	�9B	�3B	�-B	�?B	�?B	�FB	�RB	�^B	�dB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	B	B	ÖB	ÖB	ÖB	ÖB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
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
DB
DB
PB
PB
JB
JB
DB
JB
VB
\B
\B
bB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
,B
1'B
9XB
?}B
D�B
K�B
S�B
[#B
`BB
cTB
ffB
k�B
o�B
t�B
y�B
|�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�cB	�aB	�dB	�`B	�`B	�WB	�XB	�~B	��B
B
NB
I�B
kgB
j`B
keB
loB
o}B
u�B
|�B
|�B
o~B
c3B
b0B
b,B
e?B
kgB
p�B
t�B
y�B
{�B
~�B
{�B
�1B
�?B
�<B
�MB
��B
��B
��B
��B
��B
�rB
�]B
��B	B!�BE{BQ�Bd6B~�B�5B��B̧B�+B�<B�3B�(B�,B�CB�<B�nB�PB�$B�B��B�5B��BͩBȈB��B��B��B��B�}B�YB��Bf=BN�B6BTB
�-B
˟B
�YB
��B
��B
��B
��B
�B
x�B
f>B
EwB
,B	�6B	�gB	��B	�JB	y�B	f@B	T�B	L�B	DwB	5B	$�B	�B	[B	+B��B�B�VB�>B�8B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�gB�bB�oB�oB�}B��B��B��B��B��B��B�B�IB�B��B��B�5B�hB�\B�aB�pB�B��B��B��B��B��B��B�B�B��B		B	BB	JB	OB	[B	nB	�B	 �B	#�B	%�B	&�B	$�B	#�B	!�B	�B	xB	�B	 �B	!�B	!�B	"�B	#�B	&�B	)�B	*�B	+�B	-�B	0�B	6B	5B	?OB	I�B	K�B	L�B	N�B	M�B	R�B	Y�B	e0B	iLB	iKB	jNB	kWB	lZB	m_B	ooB	prB	s�B	u�B	v�B	y�B	y�B	z�B	~�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	|�B	|�B	|�B	|�B	{�B	|�B	~�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	� B	�%B	�.B	�"B	�.B	�KB	�bB	�lB	�iB	�nB	�lB	�uB	�qB	�rB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�	B	�B	�B	�B	�)B	�0B	�4B	�3B	�<B	�<B	�EB	�DB	�GB	�UB	�\B	�[B	�^B	�`B	�`B	�_B	�uB	�zB	�yB	ȀB	�}B	ɅB	ʌB	˓B	̕B	͝B	͝B	ΤB	ΣB	΢B	΢B	΢B	ΤB	ΣB	ϩB	ЯB	ЯB	ЮB	ѺB	ҾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�.B	�9B	�6B	�>B	�<B	�DB	�BB	�CB	�CB	�@B	�PB	�OB	�NB	�VB	�]B	�aB	�bB	�hB	�sB	�sB	�tB	�yB	�zB	�zB	�B	�B	�B	�B	�B	�B	�~B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
	 B

B
B
B
B
B
B
B
B
B
B
%B
#B
*B
7B
>B
?B
@B
BB
BB
GB
PB
PB
[B
bB
bB
dB
hB
dB
bB
iB
oB
eG�O�B
oB
%�B
+�B
0�B
9B
?CB
DaB
K�B
S�B
Z�B
`	B
cB
f-B
kKB
odB
t�B
y�B
|�B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451182016080714511820160807145118  AO  ARCAADJP                                                                    20150610091640    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150610091640  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150610091640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145118  IP                  G�O�G�O�G�O�                