CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-23T20:17:21Z AOML 3.0 creation; 2016-08-07T21:51:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160223201721  20160807145127  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               jA   AO  5287_9017_106                   2C  D   APEX                            6529                            072314                          846 @ח�n��
1   @ח���@0߾vȴ9�d��1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    jA   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B���C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dyl�D��D�Y�D���D��3D�3D�<�D�y�D�� D�fD�L�D�s3D�� D�  D�FfD�s3D��3D��fD�9�D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @^{@�
=@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBRG�BY�HBa�HBi�HBq�HBy�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B�#�B��qC xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRC��CxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDtw�Dy��D���D�h�D���D��=D�"=D�K�D���D��
D�%pD�[�D��=D��
D�
D�UpDڂ=D��=D�pD�H�D�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��;A��;A��;A��#A��`A��TA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�A�A�A�A�A�A�A�A�%A�A�%A�
=A�1A�
=A�
=A�
=A�JA��A� �A�"�A�(�A�9XA�|�A�jAΰ!A�bNA͏\A�-A���AƃA��HAÝ�A��yA���A�K�A��hA��A�-A�ffA�JA��jA�~�A�l�A��hA��A�p�A�n�A�?}A��A�K�A��A��#A�ffA�ƨA���A��A���A��PA���A��/A��A�5?A��A�|�A�p�A��hA��A�jA���A��;A��A��A���A��PA���A��A�hsA���A� �A�ĜA���A�VA���A���A�(�A|I�Av=qAq/Al��AhAd�\A_��A\�\AZȴAW��AT�AR��APffAO��AOAM&�AK/AI�AGp�AD��A?��A=+A;�A:�RA:bNA8ȴA6�A5
=A3��A3�A25?A1��A0z�A/�
A/
=A.ZA-�;A,��A+�#A*�/A*~�A*-A)K�A(�A(JA'�A&�A%l�A$^5A#+A!�7A�mA��A�wAVA�TAƨA��A33Ap�A��AE�A�A$�A�A��A$�A��A��A�DAn�A^5A=qA�A{A�A�wA��A�A�wA��A�wA�hAl�A?}AVA%A�`An�A �A�;A�FA�A�7A"�A�\A{A��AoA�DA�^AdZAhsA��AhsA��A �AbA1A�
A
=A5?AƨAp�A&�A%A�A�/A��A�AhsA`BA
=A
�uA
-A	�hA	C�A	�A�A�RA�DAv�A=qA��A?}AffA��A7LA�jAA  A�A��A��AdZAS�AG�A"�AA�yA�uA�
A?}A ��A @�p�@�dZ@���@��@�^5@�9X@���@���@�-@��@�=q@���@��@�A�@���@�\@�n�@�J@�`B@���@�?}@��@�@�G�@畁@�"�@��@��H@�R@�R@�R@�\@�ff@�J@��@�  @㝲@�@�t�@��y@��#@�1'@��y@��@���@��@�X@܋D@۝�@�K�@��@ڸR@�;d@�@؛�@؃@��#@�ƨ@Ӆ@�|�@�K�@�+@�ff@��#@��@�r�@�(�@��;@�33@Χ�@�~�@�G�@���@�Q�@���@�@ȋD@���@��@�9X@��
@�33@�+@�C�@�|�@ȼj@���@�%@���@ȓu@ǶF@�l�@��@�-@���@Ĵ9@�Q�@�(�@��m@�t�@���@�@�7L@��`@��D@�(�@��m@�ƨ@�dZ@�=q@���@�1'@�o@�~�@�J@��@���@��T@�M�@��+@�n�@�{@�J@�=q@�E�@��@���@��@��T@�@�?}@���@�33@�5?@��@�hs@�X@��@��`@���@���@�z�@���@��
@��@��y@��^@���@�ƨ@��R@���@�V@��@�  @���@��w@�
=@�ff@�@���@�x�@�&�@�r�@�  @��m@���@�+@��\@�@��-@�X@���@���@���@��D@�j@�Q�@�I�@�9X@�(�@��@��;@��w@�S�@��@��@�v�@�{@���@�G�@��`@��@�bN@�9X@��m@�t�@�
=@���@�=q@���@�p�@��@�V@���@��@��@��j@�9X@��@�ƨ@�;d@��@�ȴ@��!@�v�@�r�@��@�$�@���@~��@vV@oK�@gl�@^ff@Vv�@Ol�@FE�@=@6�+@-��@)��@#�@ �`@j@�`@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��/A��;A��;A��;A��#A��`A��TA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�A�A�A�A�A�A�A�A�%A�A�%A�
=A�1A�
=A�
=A�
=A�JA��A� �A�"�A�(�A�9XA�|�A�jAΰ!A�bNA͏\A�-A���AƃA��HAÝ�A��yA���A�K�A��hA��A�-A�ffA�JA��jA�~�A�l�A��hA��A�p�A�n�A�?}A��A�K�A��A��#A�ffA�ƨA���A��A���A��PA���A��/A��A�5?A��A�|�A�p�A��hA��A�jA���A��;A��A��A���A��PA���A��A�hsA���A� �A�ĜA���A�VA���A���A�(�A|I�Av=qAq/Al��AhAd�\A_��A\�\AZȴAW��AT�AR��APffAO��AOAM&�AK/AI�AGp�AD��A?��A=+A;�A:�RA:bNA8ȴA6�A5
=A3��A3�A25?A1��A0z�A/�
A/
=A.ZA-�;A,��A+�#A*�/A*~�A*-A)K�A(�A(JA'�A&�A%l�A$^5A#+A!�7A�mA��A�wAVA�TAƨA��A33Ap�A��AE�A�A$�A�A��A$�A��A��A�DAn�A^5A=qA�A{A�A�wA��A�A�wA��A�wA�hAl�A?}AVA%A�`An�A �A�;A�FA�A�7A"�A�\A{A��AoA�DA�^AdZAhsA��AhsA��A �AbA1A�
A
=A5?AƨAp�A&�A%A�A�/A��A�AhsA`BA
=A
�uA
-A	�hA	C�A	�A�A�RA�DAv�A=qA��A?}AffA��A7LA�jAA  A�A��A��AdZAS�AG�A"�AA�yA�uA�
A?}A ��A @�p�@�dZ@���@��@�^5@�9X@���@���@�-@��@�=q@���@��@�A�@���@�\@�n�@�J@�`B@���@�?}@��@�@�G�@畁@�"�@��@��H@�R@�R@�R@�\@�ff@�J@��@�  @㝲@�@�t�@��y@��#@�1'@��y@��@���@��@�X@܋D@۝�@�K�@��@ڸR@�;d@�@؛�@؃@��#@�ƨ@Ӆ@�|�@�K�@�+@�ff@��#@��@�r�@�(�@��;@�33@Χ�@�~�@�G�@���@�Q�@���@�@ȋD@���@��@�9X@��
@�33@�+@�C�@�|�@ȼj@���@�%@���@ȓu@ǶF@�l�@��@�-@���@Ĵ9@�Q�@�(�@��m@�t�@���@�@�7L@��`@��D@�(�@��m@�ƨ@�dZ@�=q@���@�1'@�o@�~�@�J@��@���@��T@�M�@��+@�n�@�{@�J@�=q@�E�@��@���@��@��T@�@�?}@���@�33@�5?@��@�hs@�X@��@��`@���@���@�z�@���@��
@��@��y@��^@���@�ƨ@��R@���@�V@��@�  @���@��w@�
=@�ff@�@���@�x�@�&�@�r�@�  @��m@���@�+@��\@�@��-@�X@���@���@���@��D@�j@�Q�@�I�@�9X@�(�@��@��;@��w@�S�@��@��@�v�@�{@���@�G�@��`@��@�bN@�9X@��m@�t�@�
=@���@�=q@���@�p�@��@�V@���@��@��@��j@�9X@��@�ƨ@�;d@��@�ȴ@��!G�O�@�r�@��@�$�@���@~��@vV@oK�@gl�@^ff@Vv�@Ol�@FE�@=@6�+@-��@)��@#�@ �`@j@�`@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�B
�B
�B
�!B
�3B
�?B
�FB
�LB
�qB
�/BVB��B�sB��B�B7LB`BBu�B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B�uB�PB�Bs�Bn�BffB`BBQ�B9XB)�B �BoBB�B�TB��B��B��B��B��B��B�TB�BBPB�BPB�B�B��BÖB�LB�B}�BPB
�uB
F�B
�B	�B	ƨB	��B	y�B	^5B	B�B	/B	'�B	�B	oB	+B��B�B�B�`B�/B�B��B��BB�wB�dB�^B�^B�RB�^B�qBǮB��B�;B�B��B	%B	1B	bB	�B	"�B	6FB	=qB	S�B	\)B	^5B	`BB	aHB	hsB	o�B	p�B	w�B	z�B	|�B	{�B	~�B	~�B	�=B	��B	��B	��B	��B	�B	�jB	��B	B	��B	��B	�/B	�yB	�B	�B	�B	�B	�B	��B	��B	��B
B

=B
JB
\B
uB
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
%�B
&�B
'�B
(�B
(�B
(�B
+B
-B
,B
+B
)�B
'�B
#�B
"�B
&�B
.B
/B
/B
/B
/B
.B
-B
-B
,B
-B
-B
-B
-B
-B
-B
+B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
#�B
#�B
!�B
 �B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
JB
1B
B	��B	��B	�B	��B
B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�;B	��B	ȴB	��B	ȴB	ŢB	ŢB	��B	��B	�B	�B	��B	��B	�B	�/B	�BB	�HB	�HB	�NB	�TB	�sB	�B	�B	�B	�B	�B	�B	�sB	�`B	�`B	�`B	�`B	�mB	�mB	�`B	�NB	�HB	�;B	�NB	�B	�B	�ZB	�NB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�/B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�`B	�ZB	�TB	�TB	�ZB	�`B	�TB	�NB	�TB	�;B	�)B	�/B	�)B	�/B	�/B	�;B	�`B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
	7B
+B
JB
�B
�B
"�B
+B
.B
33B
:^B
A�B
F�B
M�B
S�B
YB
_;B
cTB
hsB
l�B
p�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
�B
�B
�B
�#B
�(B
�PB
�BU�B��B�MB��BjB7%B`Bu�B�{B��B��B��B��B��B��B��B��B��B��B��B��B�dB�^B�WB�JB�(B��Bs�BnrBf<B`BQ�B9-B)�B �BAB�B�B�(B��BлBϲBήB˚B̢B�*B�wB �BB[B"B�qB��B��B�hB�B��B}�B"B
�IB
F�B
gB	�B	ƃB	��B	y�B	^B	BkB	.�B	'�B	�B	MB		B��B�{B�bB�AB�B��B��BʡB�mB�XB�CB�=B�>B�1B�>B�OBǉB��B�B�lB��B	 B	B	<B	�B	"�B	6B	=IB	S�B	\ B	^B	`B	a B	hKB	osB	pyB	w�B	z�B	|�B	{�B	~�B	~�B	�B	�qB	�uB	��B	��B	��B	�=B	�VB	�_B	�UB	ʒB	� B	�LB	�aB	�]B	�gB	�zB	�B	�B	��B	��B
�B

B
B
)B
CB
PB
TB
[B
bB
tB
 �B
!�B
!�B
"�B
%�B
&�B
'�B
(�B
(�B
(�B
*�B
,�B
+�B
*�B
)�B
'�B
#�B
"�B
&�B
-�B
.�B
.�B
.�B
.�B
-�B
,�B
,�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
*�B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
#�B
#�B
!�B
 �B
�B
yB
gB
VB
8B
[B
mB
kB
gB
aB
`B
[B
SB
NB
FB
7B
B
�B
�B	��B	��B	�}B	��B
�B	��B	�B	�XB	�B	�B	�B	��B	��B	�wB	�	B	жB	ȄB	ʏB	ȃB	�sB	�pB	͢B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�>B	�cB	�dB	�eB	�fB	�`B	�PB	�?B	�.B	�.B	�.B	�,B	�;B	�8B	�.B	�B	�B	�B	�B	�YB	�LB	�(B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ϫB	˔B	̘B	ϭB	��B	��B	��B	��B	��B	��B	�EB	�_B	�gB	�iB	�kB	�jB	�cB	�bB	�]B	�YB	�XB	�WB	�OB	�PB	�IB	�EB	�3B	�+B	�$B	�"B	�!B	�$B	�,B	� B	�B	�!B	�B	��B	��B	��B	��B	��B	�B	�,B	�PB	�UB	�XB	�pB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�nB	�cB	�[B	�QB	�TB	�\B	�eB	�bB	�aB	�cB	�^B	�]B	�XB	�WB	�ZB	�\B	�[B	�aB	�jB	�oB	�vB	�{B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�G�O�B
�B
B
RB
B
"�B
*�B
-�B
2�B
:&B
APB
FnB
M�B
S�B
X�B
_B
cB
h:B
lQB
pmB
s~B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451272016080714512720160807145127  AO  ARCAADJP                                                                    20160223201721    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160223201721  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160223201721  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145127  IP                  G�O�G�O�G�O�                