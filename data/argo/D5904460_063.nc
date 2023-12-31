CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-24T02:15:24Z AOML 3.0 creation; 2016-08-07T21:17:39Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150724021524  20160807141739  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ?A   AO  5285_8895_063                   2C  D   APEX                            6487                            072314                          846 @�b4~� 1   @�b5@@*}�E���c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ?A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D�fD�P D�� D�ٚD�  D�I�D�y�D���D�3D�C3D���D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��B�#�B�#�B��B��qB��B��B��B�W
B��B��qB��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��C xRC^�CxRCxRCxRC
xRCxRCxRCxRCxRCxRC��CxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6^�C8xRC:xRC<xRC>xRC@xRCBxRCDxRCF��CHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dy�D�%pD�_
D��
D��D�
D�X�D���D���D�"=D�R=D���D��
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�FA�9A�-A�-A�FA�FA�9A�-A�A�A��A��A��A��A��A���A���A��A��A��A���A���A���A��A�l�A�C�A���A�K�Aޝ�A�C�A���A�1'A��mA�ȴA� �A؃A֝�A�|�A��`Aѥ�AоwA��A�$�A�G�A�ƨA�Aȏ\AǬA��A��AŋDA�XA��A�v�A�I�A�v�A�^5A� �A���A��A�oA��A��-A���A�7LA�;dA��9A��A��A���A�1A��DA�|�A��A���A�VA���A�v�A�|�A��A��PA���A�?}A�VA�-A{�At-Am��Aj��Agp�Ab��A_x�A]�wAV1'AQ�AL��AJr�AI��AH-AF�9AEO�ABn�A?��A>A<�yA:��A8�+A5��A2ZA0v�A.�\A-�-A,�+A-dZA,n�A);dA'%A$�A"^5A�PA�uAA��A%A1A��A�mA�PA��A5?A�AQ�A-A�;A��Al�AoA��AE�A�AA�A��A|�A/A%A�AQ�A��A�AbAjA|�A`BA�A��AoA�\A��A33A;dAoA�uA(�AO�A�A�AbA��A�A��A
=A��A^5A�Al�A7LA��A��Ar�A��AC�A
=A
��A
�A
�HA
�+A	��A	|�A	p�A	hsA	33A�RA1'A��A?}A��AA�A�A��AXA�A��A�9A�AA5?A��AO�A ��A �RA �\A n�A �@�dZ@���@��@��9@��F@���@�S�@��@�n�@�/@�1@�K�@���@��@��@��@�V@��j@�Z@�  @�w@�l�@��@���@�9@�b@�K�@�R@���@�`B@�u@�b@�t�@�E�@�O�@���@�r�@���@��;@��
@��@�dZ@��@�n�@���@��@�&�@�bN@�9X@�1@��@��y@◍@���@�G�@���@��@�@��`@��m@���@�~�@�X@�r�@�  @׶F@׾w@ׅ@�S�@�"�@���@�V@պ^@��@�1'@�1@ӥ�@Ұ!@�-@�@�O�@��@���@Ѓ@Ͼw@�dZ@�~�@��`@�b@��;@���@�|�@���@�$�@ɑh@�&�@Ȭ@�Q�@�t�@��y@��H@Ə\@�5?@Ų-@�X@ēu@�  @Ý�@��y@�ff@�-@���@���@��h@�p�@�/@���@��@��@��w@��P@�dZ@�K�@�"�@�ȴ@��@��@�&�@��9@� �@��F@�|�@�dZ@�;d@�@���@�ȴ@��+@�5?@��@���@�`B@��@�Z@��F@�K�@��y@�5?@��@���@�@��^@�x�@���@��u@��u@�z�@���@���@�9X@��@�dZ@�
=@�@�{@��@�@���@���@��@�r�@� �@��
@�C�@�
=@���@��+@�=q@���@��^@�x�@�V@�j@���@�|�@�33@�ȴ@���@�5?@��@���@��@�Q�@��
@�t�@�33@��@��H@���@���@��@��j@�bN@�b@��;@���@�C�@�"�@���@�E�@�J@���@�V@�Ĝ@�z�@�b@��P@�K�@�"�@���@�V@���@�X@��@��@��u@�(�@��@��w@�S�@��@�ȴ@�v�@��@���@��@�bN@�b@���@�\)@�;d@�
=@���@�ff@�=q@��@���@��^@�hs@�?}@��@��`@�Ĝ@��@�z�@��@���@��;@�ƨ@��@���@�K�@�@�ȴ@�v�@�E�@���@��-@�`B@���@�r�@��m@��@�t�@�S�@�z�@�n�@�$�@w�;@pb@e?}@]��@V5?@N5?@F5?@?��@7l�@0 �@)��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�FA�9A�-A�-A�FA�FA�9A�-A�A�A��A��A��A��A��A���A���A��A��A��A���A���A���A��A�l�A�C�A���A�K�Aޝ�A�C�A���A�1'A��mA�ȴA� �A؃A֝�A�|�A��`Aѥ�AоwA��A�$�A�G�A�ƨA�Aȏ\AǬA��A��AŋDA�XA��A�v�A�I�A�v�A�^5A� �A���A��A�oA��A��-A���A�7LA�;dA��9A��A��A���A�1A��DA�|�A��A���A�VA���A�v�A�|�A��A��PA���A�?}A�VA�-A{�At-Am��Aj��Agp�Ab��A_x�A]�wAV1'AQ�AL��AJr�AI��AH-AF�9AEO�ABn�A?��A>A<�yA:��A8�+A5��A2ZA0v�A.�\A-�-A,�+A-dZA,n�A);dA'%A$�A"^5A�PA�uAA��A%A1A��A�mA�PA��A5?A�AQ�A-A�;A��Al�AoA��AE�A�AA�A��A|�A/A%A�AQ�A��A�AbAjA|�A`BA�A��AoA�\A��A33A;dAoA�uA(�AO�A�A�AbA��A�A��A
=A��A^5A�Al�A7LA��A��Ar�A��AC�A
=A
��A
�A
�HA
�+A	��A	|�A	p�A	hsA	33A�RA1'A��A?}A��AA�A�A��AXA�A��A�9A�AA5?A��AO�A ��A �RA �\A n�A �@�dZ@���@��@��9@��F@���@�S�@��@�n�@�/@�1@�K�@���@��@��@��@�V@��j@�Z@�  @�w@�l�@��@���@�9@�b@�K�@�R@���@�`B@�u@�b@�t�@�E�@�O�@���@�r�@���@��;@��
@��@�dZ@��@�n�@���@��@�&�@�bN@�9X@�1@��@��y@◍@���@�G�@���@��@�@��`@��m@���@�~�@�X@�r�@�  @׶F@׾w@ׅ@�S�@�"�@���@�V@պ^@��@�1'@�1@ӥ�@Ұ!@�-@�@�O�@��@���@Ѓ@Ͼw@�dZ@�~�@��`@�b@��;@���@�|�@���@�$�@ɑh@�&�@Ȭ@�Q�@�t�@��y@��H@Ə\@�5?@Ų-@�X@ēu@�  @Ý�@��y@�ff@�-@���@���@��h@�p�@�/@���@��@��@��w@��P@�dZ@�K�@�"�@�ȴ@��@��@�&�@��9@� �@��F@�|�@�dZ@�;d@�@���@�ȴ@��+@�5?@��@���@�`B@��@�Z@��F@�K�@��y@�5?@��@���@�@��^@�x�@���@��u@��u@�z�@���@���@�9X@��@�dZ@�
=@�@�{@��@�@���@���@��@�r�@� �@��
@�C�@�
=@���@��+@�=q@���@��^@�x�@�V@�j@���@�|�@�33@�ȴ@���@�5?@��@���@��@�Q�@��
@�t�@�33@��@��H@���@���@��@��j@�bN@�b@��;@���@�C�@�"�@���@�E�@�J@���@�V@�Ĝ@�z�@�b@��P@�K�@�"�@���@�V@���@�X@��@��@��u@�(�@��@��w@�S�@��@�ȴ@�v�@��@���@��@�bN@�b@���@�\)@�;d@�
=@���@�ff@�=q@��@���@��^@�hs@�?}@��@��`@�Ĝ@��@�z�@��@���@��;@�ƨ@��@���@�K�@�@�ȴ@�v�@�E�@���@��-@�`B@���@�r�@��m@��@�t�G�O�@�z�@�n�@�$�@w�;@pb@e?}@]��@V5?@N5?@F5?@?��@7l�@0 �@)��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	bNB	aHB	aHB	bNB	aHB	bNB	bNB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	aHB	`BB	`BB	_;B	^5B	\)B	W
B	P�B	N�B	M�B	I�B	E�B	F�B	H�B	dZB	q�B	�B	��B	��B	ŢB	�B
]/B
�B
�qB(�B��B�B%�BW
Bu�B�B�B�VB��B�B��B��B��B��B��B��B��B�BJ�BJ�BaHBYBT�B49B�yB�B�BR�B�B
��B
�B
�B
�
B
B
��B
�JB
_;B
�B

=B	�B	�B	s�B	M�B	7LB	#�B	hB	B�B�#B��B��B��B��B��B�
B�B�NB�B��B��B��B	B	!�B	�B	,B	.B	:^B	J�B	m�B	hsB	XB	H�B	6FB	+B	�B	�B	9XB	?}B	>wB	A�B	M�B	^5B	`BB	dZB	hsB	o�B	|�B	�B	�+B	�JB	��B	��B	��B	��B	��B	�?B	�?B	�FB	�dB	��B	��B	B	��B	�}B	�jB	�LB	�?B	�LB	��B	��B	��B	��B	��B	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B
B
B
B
  B
B
B
1B
	7B

=B
DB
DB
+B
B
B
B
B
B
+B
1B
1B
	7B
B
B
B
B
%B
%B
%B
1B
1B
1B
1B
+B
+B
%B
%B
%B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
JB
JB
JB
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
JB
JB
JB
DB
DB

=B

=B
DB
PB
VB
PB
VB
PB
PB
PB
PB
PB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
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
oB
oB
oB
uB
uB
{B
uB
{B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
!�B
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
"�B
%�B
.B
0!B
7LB
;dB
C�B
I�B
N�B
W
B
\)B
_;B
bNB
hsB
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	b9B	a3B	a4B	b;B	a1B	b9B	b9B	a5B	a4B	a7B	a1B	a5B	a1B	a7B	a5B	a2B	a2B	a2B	a7B	a5B	a2B	`-B	`/B	_(B	^!B	\B	V�B	P�B	N�B	M�B	I�B	E�B	F�B	H�B	dDB	q�B	��B	�pB	��B	ŇB	�B
]B
��B
�PB(�B��B�B%�BV�Bu�B��B��B�,B�{B��B��B��B�kB�_B�mB�WB�sB��BJ�BJ�BaBX�BT�B4B�MB��B��BR�BTB
��B
�XB
��B
��B
�dB
��B
�"B
_B
�B

B	�}B	��B	s�B	M�B	7)B	#�B	GB	 �B�B�B��B��B��B��B��B��B��B�,B�tB��B��B��B	�B	!�B	}B	+�B	-�B	:7B	J�B	mfB	hLB	W�B	H�B	6B	*�B	�B	�B	90B	?TB	>OB	AaB	M�B	^	B	`B	d.B	hIB	orB	|�B	��B	��B	�B	�YB	�lB	��B	��B	��B	�B	�B	�B	�7B	�ZB	�WB	�aB	�[B	�MB	�:B	�B	�B	�B	�TB	̛B	ʓB	ͣB	��B	�*B	�6B	�>B	�JB	�TB	�mB	�yB	�B	�tB	�nB	�{B	�B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B
 �B
�B
�B	��B
�B
�B
�B
	B

B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
 B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�xB	�xB	�tB	�B	��B	��B	��B	��B	�B	�B	�B	�{B	�}B	�{B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B
	 B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B

B

B
B
B
B
B
 B
B
B
B
B
B
B
B
B
B
B
 B
B
&B
'B
$B
%B
$B
#B
,B
-B
-B
,B
,B
+B
.B
-B
0B
0B
6B
7B
7B
?B
@B
AB
?B
DB
KB
CB
JB
EB
QB
OB
NB
UB
VB
_B
\B
\B
[B
]B
\B
bB
iB
oB
hB
iB
iB
hB
eB
hB
iB
�B
�B
!�B
!�B
!�B
!�B
!�B
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
 �G�O�B
%�B
-�B
/�B
7B
;0B
C_B
I�B
N�B
V�B
[�B
_B
bB
h9B
mW1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417392016080714173920160807141739  AO  ARCAADJP                                                                    20150724021524    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150724021524  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150724021524  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141739  IP                  G�O�G�O�G�O�                