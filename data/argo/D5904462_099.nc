CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-17T20:17:14Z AOML 3.0 creation; 2016-08-07T21:51:25Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160117201714  20160807145125  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               cA   AO  5287_9017_099                   2C  D   APEX                            6529                            072314                          846 @׎���J1   @׎�s���@/s�E����d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    cA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx��B��B�  B�ffB�ffB�  B�ffB���B���B�  B�  B�  B�  B�33B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyffD��D�@ D�ffD��fD�	�D�P D��D��3D� D�I�D�y�D�� D�3D�3Dڌ�D�ɚD��D�fD�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�
=@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBrG�Bz�B��qB��B�W
B�W
B��B�W
B��qB��qB��B��B��B��B�#�B�#�B��>B��qB��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRC^�C^�CxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCD��CFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�D$zD�zDD�DD�DD�DD�DD�DD�D$zD�DD�DD�DD�DD��DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'�D'�D(D(�D)�D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\$zD\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�GDy�zD��D�O
D�upD��pD��D�_
D�+�D��=D�
D�X�D���D��
D�=D�"=Dڛ�D�ؤD���D�%pD�h�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ÁA̓ÁA̓A̓AͅA̓ÁAͅA̓ÁA�|�A�~�A͋DA͏\A͏\A͏\A͑hA͑hA͑hA͓uA͓uA͕�A͓uA͕�A͕�A͕�A͕�A͕�A͏\A͓uA͗�A�JA�p�A�%A�ĜA��A��jA���A��wA���A�;dA���A�jA���A�$�A�;dA�hsA}
=AxA�Ar1'Am�mAk��AhbAd��Ac�;Ac?}AaA`�9A^v�A\��A[G�AW&�AShsARJANz�AI��AG+ACAA�TA@1A>bA;�^A:~�A9oA7;dA5x�A4�9A4(�A2��A.�DA-l�A-;dA,�A,(�A+33A)+A&�A&�!A&v�A&1'A$�jA$JA#��A#S�A"�jA"z�A!�hA/AE�A�9AXA|�A��AA=qA~�A"�A
=A�/A5?A�\AA�A��A`BA+A�A�A�A��A��A�hA��A�PA�A�AA�A��AĜAĜA�hAA�+A�
A�FA��AK�A
��A
bA	7LAffA�7A��AdZA�A �\@���@�5?@���@�@���@�/@@���@�7L@��m@띲@��@�/@�F@���@�+@�p�@�$�@�X@��@��m@އ+@��@�@�/@ܣ�@�1'@�\)@��@ԣ�@�-@���@��`@ЋD@��@�|�@�^5@��@��@˅@�n�@ʟ�@�5?@ɩ�@�G�@��/@���@���@��@��@�7L@�ff@��`@��@�j@˅@�E�@��;@��@�"�@��@�ƨ@�|�@���@�?}@�ȴ@�;d@ˍP@ˮ@˕�@�C�@���@�V@���@�M�@��H@�o@ʟ�@ʟ�@ʇ+@�V@�-@ə�@ȣ�@��@��@�|�@�o@Ɨ�@�J@Ų-@őh@�hs@�/@���@�r�@��@�@�v�@���@���@�?}@��@�I�@�1@���@�E�@�X@���@��@�~�@�/@�9X@��;@�l�@�
=@��@�ff@�-@�$�@��T@��@�?}@��@�&�@���@���@�&�@��`@��@�?}@�hs@�G�@��@���@��u@���@��@�  @��m@��;@��
@��@�K�@�o@�o@�@�$�@�O�@��@���@�%@��@��@�V@�&�@�%@��@���@�b@�1'@�9X@�1'@�l�@�;d@�33@�33@��y@��\@�~�@�ff@���@���@��-@�X@���@�z�@��@��P@�ȴ@��!@��!@���@�v�@�E�@��@��@�{@���@���@���@���@��`@���@�r�@� �@��@��P@�
=@���@���@��!@�~�@�M�@���@�Ĝ@�9X@���@��P@�S�@�
=@�$�@�@��#@���@�p�@�?}@�7L@�7L@��@��@���@��@��R@���@��T@�X@���@��@��@�l�@�S�@�;d@���@�E�@��T@���@��@�Ĝ@�9X@��
@��@�K�@��R@�~�@�n�@�5?@�5?@�@��^@�/@��@���@���@��@�b@�ƨ@�|�@�K�@�ȴ@�v�@�^5@�E�@�-@�@���@�x�@�G�@��@�Z@��@��;@��;@�ƨ@�t�@�33@��@��y@���@�n�@�M�@�5?@�@���@���@��h@��h@��@�`B@�G�@�7L@��@���@��`@���@��@�Z@� �@�ƨ@��P@�\)@�@�ȴ@��R@��R@���@�5?@��-@�O�@���@�Ĝ@��@�r�@��@���@��
@��w@�|�@�C�@�C�@�C�@�C�@�
=@��@���@�V@�@���@��7@�X@�V@��`@�z�@�9X@|�@~��@~@}p�@|z�@{o@w��@k�
@a��@X�9@Q%@F�+@<�@5@0b@)�^@%�@ ��@�@\)@M�@�+@��@	�@;d@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111ÁA̓ÁA̓A̓AͅA̓ÁAͅA̓ÁA�|�A�~�A͋DA͏\A͏\A͏\A͑hA͑hA͑hA͓uA͓uA͕�A͓uA͕�A͕�A͕�A͕�A͕�A͏\A͓uA͗�A�JA�p�A�%A�ĜA��A��jA���A��wA���A�;dA���A�jA���A�$�A�;dA�hsA}
=AxA�Ar1'Am�mAk��AhbAd��Ac�;Ac?}AaA`�9A^v�A\��A[G�AW&�AShsARJANz�AI��AG+ACAA�TA@1A>bA;�^A:~�A9oA7;dA5x�A4�9A4(�A2��A.�DA-l�A-;dA,�A,(�A+33A)+A&�A&�!A&v�A&1'A$�jA$JA#��A#S�A"�jA"z�A!�hA/AE�A�9AXA|�A��AA=qA~�A"�A
=A�/A5?A�\AA�A��A`BA+A�A�A�A��A��A�hA��A�PA�A�AA�A��AĜAĜA�hAA�+A�
A�FA��AK�A
��A
bA	7LAffA�7A��AdZA�A �\@���@�5?@���@�@���@�/@@���@�7L@��m@띲@��@�/@�F@���@�+@�p�@�$�@�X@��@��m@އ+@��@�@�/@ܣ�@�1'@�\)@��@ԣ�@�-@���@��`@ЋD@��@�|�@�^5@��@��@˅@�n�@ʟ�@�5?@ɩ�@�G�@��/@���@���@��@��@�7L@�ff@��`@��@�j@˅@�E�@��;@��@�"�@��@�ƨ@�|�@���@�?}@�ȴ@�;d@ˍP@ˮ@˕�@�C�@���@�V@���@�M�@��H@�o@ʟ�@ʟ�@ʇ+@�V@�-@ə�@ȣ�@��@��@�|�@�o@Ɨ�@�J@Ų-@őh@�hs@�/@���@�r�@��@�@�v�@���@���@�?}@��@�I�@�1@���@�E�@�X@���@��@�~�@�/@�9X@��;@�l�@�
=@��@�ff@�-@�$�@��T@��@�?}@��@�&�@���@���@�&�@��`@��@�?}@�hs@�G�@��@���@��u@���@��@�  @��m@��;@��
@��@�K�@�o@�o@�@�$�@�O�@��@���@�%@��@��@�V@�&�@�%@��@���@�b@�1'@�9X@�1'@�l�@�;d@�33@�33@��y@��\@�~�@�ff@���@���@��-@�X@���@�z�@��@��P@�ȴ@��!@��!@���@�v�@�E�@��@��@�{@���@���@���@���@��`@���@�r�@� �@��@��P@�
=@���@���@��!@�~�@�M�@���@�Ĝ@�9X@���@��P@�S�@�
=@�$�@�@��#@���@�p�@�?}@�7L@�7L@��@��@���@��@��R@���@��T@�X@���@��@��@�l�@�S�@�;d@���@�E�@��T@���@��@�Ĝ@�9X@��
@��@�K�@��R@�~�@�n�@�5?@�5?@�@��^@�/@��@���@���@��@�b@�ƨ@�|�@�K�@�ȴ@�v�@�^5@�E�@�-@�@���@�x�@�G�@��@�Z@��@��;@��;@�ƨ@�t�@�33@��@��y@���@�n�@�M�@�5?@�@���@���@��h@��h@��@�`B@�G�@�7L@��@���@��`@���@��@�Z@� �@�ƨ@��P@�\)@�@�ȴ@��R@��R@���@�5?@��-@�O�@���@�Ĝ@��@�r�@��@���@��
@��w@�|�@�C�@�C�@�C�@�C�@�
=@��@���@�V@�@���@��7@�X@�V@��`@�z�@�9X@|�@~��@~@}p�@|z�G�O�@w��@k�
@a��@X�9@Q%@F�+@<�@5@0b@)�^@%�@ ��@�@\)@M�@�+@��@	�@;d@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	(�B	)�B	<jB	T�B	�bB	��B	��B	��B	��B	��B	�hB	�PB	�1B	}�B	p�B	hsB	bNB	ZB	R�B	N�B	O�B	N�B	O�B	Q�B	P�B	O�B	P�B	O�B	T�B	VB	T�B	YB	R�B	K�B	:^B	 �B	�B	PB	+B	  B��B��B�B�B��B	B	1B	DB	1B	  B	B	+B	DB	DB	
=B	
=B	VB	\B	bB	bB	uB	uB	oB	oB	oB	hB	VB	VB	PB		7B	1B	B��B��B��B�B�fB�sB	
=B	&�B	>wB	H�B	R�B	XB	ZB	ZB	YB	cTB	q�B	r�B	q�B	p�B	gmB	R�B	E�B	=qB	49B	0!B	.B	-B	1'B	;dB	>wB	>wB	?}B	C�B	J�B	O�B	P�B	L�B	G�B	C�B	>wB	8RB	/B	�B	1B�B�B�ZB�TB�NB�ZB�B��B��B��B��B��B��B	B	B	B	  B	B	B	B	B	+B		7B	DB	\B	hB	uB	�B	\B	B��B��B��B��B	B	B		7B		7B	
=B	bB	�B	�B	%�B	(�B	)�B	-B	0!B	0!B	1'B	33B	5?B	B�B	YB	[#B	[#B	YB	S�B	K�B	G�B	J�B	XB	\)B	]/B	e`B	r�B	�B	�%B	�7B	�DB	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�3B	�?B	�dB	�jB	�jB	�jB	�jB	�jB	�wB	��B	��B	�}B	�^B	�RB	�XB	�-B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�jB	�}B	��B	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�/B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�BB	�BB	�5B	�;B	�BB	�NB	�ZB	�ZB	�fB	�fB	�fB	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
	7B
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
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
bB
hB
hB
hB
hB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
$�B
%�B
$�B
.B
6FB
<jB
B�B
K�B
R�B
XB
]/B
cTB
hsB
m�B
q�B
u�B
y�B
}�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	)�B	(�B	)�B	<RB	T�B	�EB	��B	��B	��B	�wB	�^B	�IB	�/B	�B	}�B	p�B	hQB	b-B	Y�B	R�B	N�B	O�B	N�B	O�B	Q�B	P�B	O�B	P�B	O�B	T�B	U�B	T�B	X�B	R�B	K�B	:9B	 �B	\B	/B	B��B��B��B�B�B��B	 �B	B	B	B��B	�B	B	B	B	
B	
B	/B	6B	=B	<B	OB	LB	GB	FB	GB	?B	.B	/B	&B		B		B	�B��B��B��B�_B�<B�LB	
B	&�B	>JB	H�B	R�B	W�B	Y�B	Y�B	X�B	c&B	q}B	r�B	q}B	pxB	g@B	R�B	EyB	=FB	4B	/�B	-�B	,�B	0�B	;9B	>KB	>JB	?SB	CgB	J�B	O�B	P�B	L�B	G�B	CiB	>HB	8'B	.�B	�B	B�B�UB�/B�,B�$B�3B�bB��B��B��B��B��B��B	 �B	 �B	 �B��B	 �B	 �B	 �B	�B	 B		B	B	.B	9B	EB	TB	1B	�B��B��B��B��B	 �B	�B		
B		
B	
B	4B	`B	�B	%�B	(�B	)�B	,�B	/�B	/�B	0�B	3B	5B	BaB	X�B	Z�B	Z�B	X�B	S�B	K�B	GB	J�B	W�B	[�B	\�B	e0B	r}B	��B	��B	�B	�B	�(B	�6B	�5B	�>B	�KB	�`B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�6B	�7B	�7B	�6B	�8B	�EB	�OB	�XB	�HB	�*B	�B	�#B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�EB	�UB	�jB	�zB	�}B	ʍB	̕B	ϪB	ϨB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	��B	��B	��B	��B	��B	� B	�B	� B	�B	�B	��B	�B	�B	�B	�#B	�&B	�.B	�0B	�0B	�.B	�=B	�CB	�CB	�DB	�BB	�CB	�DB	�CB	�IB	�OB	�OB	�PB	�UB	�VB	�UB	�VB	�`B	�gB	�rB	�B	�B	�}B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
 B
+B
0B
0B
0B
0B
6B
5B
6B
3B
>B
AB
GB
NB
VB
TB
TB
ZB
\B
ZB
ZB
ZB
ZB
[B
\B
ZB
dB
vB
rB
nB
mB
hB
fB
mB
lB
nB
nB
lB
B
�B
�B
 �B
 �B
 �B
!�B
!�B
"�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
$�G�O�B
$�B
-�B
6B
<2B
BWB
K�B
R�B
W�B
\�B
cB
h9B
mWB
qoB
u�B
y�B
}�B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451252016080714512520160807145125  AO  ARCAADJP                                                                    20160117201714    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160117201714  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160117201714  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145125  IP                  G�O�G�O�G�O�                