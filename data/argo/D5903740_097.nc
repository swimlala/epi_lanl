CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-11-14T13:00:52Z AOML 3.0 creation; 2016-06-01T00:08:21Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141114130052  20160531170821  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               aA   AO  4055_7112_097                   2C  D   APEX                            5374                            041511                          846 @�#E��w�1   @�#Fax7�@9��G�{�d#333331   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    aA   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�ffB���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�3D�FfD�� D���D� D�I�D�l�D��3D���D�<�D�� Dǹ�D�3D�9�D�y�D�ٚD� D�FfD�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @^{@�
=@�
=A�A'�AG�Ag�A�A�A���A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B�#�B�W
B��qB��B��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt��Dy�zD�=D�UpD��
D���D�
D�X�D�{�D��=D��D�K�D��
D�ȤD�=D�H�Dڈ�D��D�
D�UpD�=D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��yA��A��A��A��A��A��A��A���A��A���A���A��A��A��`A��TA��TA��`A��`A��`A��`A��`A��`A��TA��`A��TA��;A��/A��A���A�ȴA�A��#A�"�A�x�A���A��A�\)A�ĜA�l�A�r�A��A��yA�A���A��A��-A�-A��A��A���A���A��HA���A��TA�l�A�oA��A�5?A��A��A�n�A���A��A��+A��A��;A��A���A�$�A���A��^A��yA��-A�M�A�K�A�O�A�ƨA��DA���A��A���A�z�A�VA��7A�+A�A��A���A�Q�A���A�\)A�oA�7A~v�A|  Az �Aw|�At��Ap�HAnJAl��Aj�+AhA�Af�RAeAcK�Ab-A`9XA_+A]�A\JA[��A[��A[AZ{AYdZAW��AW/AVbNATv�AQ?}APĜAPjAP�AO�AM�;ALbAJ��AJ-AHz�AG��AG"�AF�AD��AC�PAC%AB1'AA�A?�wA>�/A>�A=��A;�A;\)A;oA:ĜA:^5A9�A97LA9%A8jA7"�A5?}A3"�A2�jA2I�A2JA1A1t�A1K�A1+A0ȴA0~�A0I�A0{A/�A/x�A-t�A+`BA*�uA*v�A*Q�A*JA)ƨA)hsA)%A(1A&ĜA&=qA%+A$�+A#��A#
=A"�A!�A!`BA 5?A�wA��Ax�AO�A;dA"�A�A�`A�9A~�A�A�mA�
AK�A`BA~�A?}Al�A��A�A�HA��Az�AbNAbA��Ap�A�A�DA�TAG�A�\A"�AI�A$�A�
A�A��A7LA�A�+AA5?AS�A r�A E�@�\)@���@��R@�t�@�\)@�r�@��T@�/@�r�@�~�@���@��@旍@�J@��`@�u@�bN@�v�@�(�@�E�@�1'@�ƨ@��H@�{@�hs@��@�
=@���@�~�@��#@��`@��`@�\)@�n�@̓u@��;@˝�@�dZ@�^5@�O�@ȴ9@Ǯ@�G�@���@�A�@��@��\@��@���@�`B@���@���@��@���@��#@�X@�Q�@���@���@�^5@���@��@��@���@�r�@�V@�X@��9@��
@��R@��!@�-@�X@�b@�dZ@�;d@�@���@�5?@�p�@��`@���@�?}@�j@��m@�S�@��@�{@�x�@�X@�7L@��@�Ĝ@��u@�I�@�9X@��@��w@��@���@�J@�X@��@�%@��j@�Z@�1@�ƨ@���@�;d@���@��@��@�bN@��F@��@��y@���@�E�@��T@�O�@��/@��u@�Q�@�  @���@�33@��y@���@�x�@�V@��j@�Q�@���@�|�@�t�@�dZ@�;d@�"�@�o@��H@�5?@��#@���@�`B@���@�z�@�r�@��@��
@���@���@��P@�t�@�33@�
=@���@��#@��-@��7@�G�@�7L@�7L@��`@���@��D@���@��@���@��@�Ĝ@���@��9@��@���@���@��D@�Z@�9X@�1'@� �@�1@��m@��
@�ƨ@��@���@��P@�|�@�l�@�\)@�C�@��R@��T@���@���@���@��7@�?}@���@���@���@�Ĝ@��D@�Q�@�(�@� �@�1@���@��@�l�@��@��@�"�@��y@�ȴ@���@���@�E�@�@��h@�7L@��@���@��/@�Ĝ@��9@��@�A�@�1'@� �@��@�b@�  @~��@~ff@}��@}O�@|��@|�j@|(�@{dZ@{"�@z�@z��@z^5@y��@y&�@x�`@x1'@w\)@w�@w
=@v�y@tj@kdZ@bn�@[�
@St�@M`B@G\)@A�@=O�@7�@1x�@,I�@$�D@E�@�!@�-@��@��@��@/@x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��yA��A��A��A��A��A��A��A���A��A���A���A��A��A��`A��TA��TA��`A��`A��`A��`A��`A��`A��TA��`A��TA��;A��/A��A���A�ȴA�A��#A�"�A�x�A���A��A�\)A�ĜA�l�A�r�A��A��yA�A���A��A��-A�-A��A��A���A���A��HA���A��TA�l�A�oA��A�5?A��A��A�n�A���A��A��+A��A��;A��A���A�$�A���A��^A��yA��-A�M�A�K�A�O�A�ƨA��DA���A��A���A�z�A�VA��7A�+A�A��A���A�Q�A���A�\)A�oA�7A~v�A|  Az �Aw|�At��Ap�HAnJAl��Aj�+AhA�Af�RAeAcK�Ab-A`9XA_+A]�A\JA[��A[��A[AZ{AYdZAW��AW/AVbNATv�AQ?}APĜAPjAP�AO�AM�;ALbAJ��AJ-AHz�AG��AG"�AF�AD��AC�PAC%AB1'AA�A?�wA>�/A>�A=��A;�A;\)A;oA:ĜA:^5A9�A97LA9%A8jA7"�A5?}A3"�A2�jA2I�A2JA1A1t�A1K�A1+A0ȴA0~�A0I�A0{A/�A/x�A-t�A+`BA*�uA*v�A*Q�A*JA)ƨA)hsA)%A(1A&ĜA&=qA%+A$�+A#��A#
=A"�A!�A!`BA 5?A�wA��Ax�AO�A;dA"�A�A�`A�9A~�A�A�mA�
AK�A`BA~�A?}Al�A��A�A�HA��Az�AbNAbA��Ap�A�A�DA�TAG�A�\A"�AI�A$�A�
A�A��A7LA�A�+AA5?AS�A r�A E�@�\)@���@��R@�t�@�\)@�r�@��T@�/@�r�@�~�@���@��@旍@�J@��`@�u@�bN@�v�@�(�@�E�@�1'@�ƨ@��H@�{@�hs@��@�
=@���@�~�@��#@��`@��`@�\)@�n�@̓u@��;@˝�@�dZ@�^5@�O�@ȴ9@Ǯ@�G�@���@�A�@��@��\@��@���@�`B@���@���@��@���@��#@�X@�Q�@���@���@�^5@���@��@��@���@�r�@�V@�X@��9@��
@��R@��!@�-@�X@�b@�dZ@�;d@�@���@�5?@�p�@��`@���@�?}@�j@��m@�S�@��@�{@�x�@�X@�7L@��@�Ĝ@��u@�I�@�9X@��@��w@��@���@�J@�X@��@�%@��j@�Z@�1@�ƨ@���@�;d@���@��@��@�bN@��F@��@��y@���@�E�@��T@�O�@��/@��u@�Q�@�  @���@�33@��y@���@�x�@�V@��j@�Q�@���@�|�@�t�@�dZ@�;d@�"�@�o@��H@�5?@��#@���@�`B@���@�z�@�r�@��@��
@���@���@��P@�t�@�33@�
=@���@��#@��-@��7@�G�@�7L@�7L@��`@���@��D@���@��@���@��@�Ĝ@���@��9@��@���@���@��D@�Z@�9X@�1'@� �@�1@��m@��
@�ƨ@��@���@��P@�|�@�l�@�\)@�C�@��R@��T@���@���@���@��7@�?}@���@���@���@�Ĝ@��D@�Q�@�(�@� �@�1@���@��@�l�@��@��@�"�@��y@�ȴ@���@���@�E�@�@��h@�7L@��@���@��/@�Ĝ@��9@��@�A�@�1'@� �@��@�b@�  @~��@~ff@}��@}O�@|��@|�j@|(�@{dZ@{"�@z�@z��@z^5@y��@y&�@x�`@x1'@w\)@w�@w
=@v�y@tj@kdZ@bn�@[�
@St�@M`B@G\)@A�@=O�@7�@1x�@,I�@$�D@E�@�!@�-@��@��@��@/@x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBy�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bx�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bu�Bu�Bt�Bt�Bs�B<jB�%B#�BoB	7B  B�B�5B��B��B��B��B��BǮBB�XB��B�{B�PB�+B~�Bm�B^5BS�BA�B/B"�B�B{BPB%BB�B�BÖB��B�bB�B}�Bn�B`BBP�BG�B5?B$�B�B{BB
�B
��B
��B
�B
��B
��B
�uB
�=B
� B
u�B
m�B
e`B
_;B
]/B
Q�B
:^B
(�B
{B	��B	�HB	��B	��B	�3B	��B	��B	�VB	�B	~�B	t�B	m�B	e`B	aHB	`BB	`BB	^5B	YB	T�B	O�B	N�B	J�B	A�B	:^B	9XB	8RB	6FB	33B	/B	$�B	�B	�B	\B	JB	
=B	%B	  B��B��B�B�B�mB�ZB�NB�;B�/B�#B�B�B�B�B�B�
B��B��BĜB��B�wB�jB�dB�^B�XB�XB�RB�LB�?B�9B�3B�-B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�DB�7B�+B�B�B� B}�Bz�By�Bx�Bx�Bw�Bw�Bw�Bv�Bv�Bu�Bt�Bs�Br�Bq�Bm�BgmBaHBYBR�BM�BJ�BH�BH�BG�BF�BE�BD�BC�BB�B@�B>wB<jB8RB33B.B,B+B+B)�B(�B'�B%�B#�B �B�B�B�B�B�B{BhB\BVBVBVBJB
=B
=B
=B	7B1B1B+B%BBBBBBBBBBBBBB  BBBB%B%B%B%B+B+B+B%B1BJB\BhBhBoBoBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B%�B&�B(�B-B,B-B/B2-B49B5?B5?B5?B7LB8RB9XB<jBH�BK�BM�BO�BQ�BT�BXBYBYBZB[#B\)B]/B]/B]/B_;B_;BbNBffBk�Bl�Bl�Bm�Bo�Bq�Br�Bs�Bt�Bu�By�B}�B�B�+B�JB�PB�hB�{B��B��B��B��B��B��B��B��B��B�B�-B�FB�RB�dB��BBÖBÖBĜBŢBŢBǮB��B��B��B�
B�B�;B�;B�TB�fB�sB�sB�sB�yB�B�B�B��B��B��B��B��B��B��B	B	+B	PB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	$�B	%�B	%�B	&�B	'�B	'�B	(�B	)�B	)�B	+B	0!B	8RB	:^B	:^B	:^B	;dB	=qB	@�B	@�B	@�B	B�B	D�B	H�B	J�B	K�B	L�B	N�B	R�B	W
B	ZB	YB	YB	\)B	]/B	^5B	_;B	cTB	jB	k�B	n�B	r�B	s�B	t�B	u�B	u�B	u�B	z�B	{�B	{�B	|�B	|�B	|�B	�B	�B	�%B	�7B	�=B	�DB	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ĜB	�)B	�B	��B
{B
�B
#�B
+B
2-B
;dB
A�B
K�B
S�B
XB
_;B
dZB
jB
o�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bx�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bu�Bu�Bt�Bt�Bs�B<NB�B#�BHB	B��B�bB�
B��BкB��B��B̠BǃB�eB�-B��B�SB�&B� B~�BmdB^	BS�BA`B.�B"�BvBOB!B�B�B�B��B�fB��B�8B��B}�BniB`BP�BG�B5B$�BuBLB�B
�kB
ʔB
�WB
��B
��B
�eB
�IB
�B
�B
u�B
mhB
e5B
_B
]B
Q�B
:4B
(�B
SB	��B	� B	̦B	�bB	�B	��B	�tB	�-B	��B	~�B	t�B	mmB	e:B	a&B	`B	`B	^B	X�B	T�B	O�B	N�B	J�B	AfB	:;B	92B	8-B	6#B	3B	.�B	$�B	�B	bB	9B	(B	
B	B��B��B��B�B�hB�IB�9B�-B�B�B��B��B��B��B��B��B��B��B̫B�|B�aB�UB�IB�BB�=B�8B�6B�0B�,B� B�B�B�	B��B��B��B��B��B��B��B��B��B�xB�bB�HB�4B�&B�B�B��B��B�B}�Bz�By�Bx�Bx�Bw�Bw�Bw�Bv�Bv�Bu�Bt�Bs�Br�Bq�BmrBgLBa'BX�BR�BM�BJ�BH�BH�BG�BF�BE�BD}BCwBBpB@eB>XB<KB84B3B-�B+�B*�B*�B)�B(�B'�B%�B#�B �B�B~B�BmBQBAB-B<BBBBB
B
B
B	B�BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�B�B�B�B�B�BB
BBB�BB:B/B-BNBKBLBRB_BgBfBkBqBvB}B�B�B�B�B�B�B�B#�B%�B&�B(�B,�B+�B,�B.�B2B4B5B5B5B7&B8,B93B<DBH�BK�BM�BO�BQ�BT�BW�BX�BX�BY�BZ�B\B]B]B]B_B_Bb'Bf?Bk^BlbBlaBmiBoxBq�Br�Bs�Bt�Bu�By�B}�B��B�B�B�%B�=B�SB�TB�cB�uB�|B��B��B��B��B��B��B�B�B�%B�5B�VB�cB�jB�jB�qB�uB�uBǀBάB��B��B��B��B�B�B�&B�7B�BB�FB�EB�LB�PB�[B�iB��B��B��B��B��B��B��B	�B	�B	B	;B	PB	jB	pB	vB	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	$�B	%�B	%�B	&�B	'�B	'�B	(�B	)�B	)�B	*�B	/�B	8"B	:,B	:,B	:/B	;2B	=AB	@QB	@RB	@SB	B^B	DmB	H�B	J�B	K�B	L�B	N�B	R�B	V�B	Y�B	X�B	X�B	[�B	\�B	^ B	_	B	c B	jJB	kRB	ndB	r{B	s�B	t�B	u�B	u�B	u�B	z�B	{�B	{�B	|�B	|�B	|�B	��B	��B	��B	�B	�B	�B	�"B	�1B	�9B	�AB	�HB	�LB	�YB	�bB	�iB	��B	��B	��B	��B	��B	��B	�eB	��B	�NB	��B
BB
bB
#�B
*�B
1�B
;*B
ARB
K�B
S�B
W�B
_B
dB
jEB
obB
s}B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708212016053117082120160531170821  AO  ARCAADJP                                                                    20141114130052    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141114130052  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141114130052  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170821  IP                  G�O�G�O�G�O�                