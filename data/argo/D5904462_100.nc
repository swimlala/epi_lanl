CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-23T03:16:27Z AOML 3.0 creation; 2016-08-07T21:51:25Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160123031627  20160807145126  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               dA   AO  5287_9017_100                   2C  D   APEX                            6529                            072314                          846 @׏���1   @׏����o@0��"���d�j~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    dA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�ffB�ffB���B�  B�  B�  B�  B�33B�  B�33B㙚B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D��D�0 D��fD��3D�fD�S3D��3D��fD�3D�L�D�s3D��fD�fD�C3Dڜ�D���D���D�,�D�p D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B�#�B�W
B�W
BĽqB��B��B��B��B�#�B��B�#�B�>B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D $zD �zDD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt�GDy�D��D�?
D��pD��=D�pD�b=D��=D��pD�"=D�[�D��=D��pD�%pD�R=Dګ�D���D���D�;�D�
D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�Q�A�E�A�A�A�=qA�7LA�5?A�33A�1'A�5?A�33A�1'A�33A�/A�1'A�1'A�33A�5?A�33A�7LA�7LA�1'A�+A�+A�-A�-A�&�A�(�A�(�A�"�A�&�A�+A�/A�+A�(�A�-A�1'A�1'A�33A�5?A�5?A�/A��A���A�Q�A��A� �A��uA�E�A�1A�n�A���A��^A�ĜA�r�A�jA�1'A�%A��A�
=A��A��`A��9A}hsAv��ArZApM�Aj�Ad�!A`�A[�mAYAVM�AR�AO�hANA�AL��AL��AL��AK�FAJ��AH��ADn�A?
=A=��A<VA9�hA6ȴA5��A4�A2ĜA/�A.��A.I�A-�^A-�7A-A,=qA*��A)�A)p�A(��A'�mA&�yA&n�A%oA$=qA#�A#"�A!�FA JA/A��A&�A��A?}A��Az�AA�A1'A�TA\)AhsAn�A��A��A�jA�wA
=AoAx�A5?Az�A�jA�RAhsA��A�yA�A��A�+A-Al�A�\AhsA
ĜA
ȴA
�A	
=AA�A��At�AO�A\)A|�A�TA��A5?A�A ��A ��@��F@��@�hs@��/@�33@��@��@��@��@�b@��H@�z�@�@�33@��@�&�@�+@���@�~�@���@�%@�Ĝ@�r�@�1'@��@�ƨ@�S�@�@�~�@���@܃@۾w@�S�@ڧ�@�x�@֧�@�V@�t�@ѡ�@��y@��@�;d@�b@���@Ь@��@��@�V@�?}@ȼj@ȓu@�9X@�t�@�\)@�&�@���@ˍP@́@�=q@�"�@�b@�(�@��@�|�@�\)@�dZ@�ȴ@Ͳ-@�/@�bN@˅@�o@ʧ�@��@��@ȼj@ȃ@�1'@�Q�@Ǯ@�dZ@�\)@�S�@�C�@�"�@��@�v�@�/@��y@�V@�V@�M�@��@��^@��`@���@��y@��j@�;d@�=q@���@�Q�@��@���@�@�x�@���@�A�@�1'@�(�@� �@��m@�ƨ@�ȴ@�5?@�J@���@��#@��T@��#@��-@�x�@��^@��T@�J@�=q@��+@��R@���@��@�^5@��@�X@��u@��@��@��m@��m@�t�@��@�~�@�{@�@�{@���@��@�Z@� �@���@�K�@�
=@�n�@��@�@�@��h@�/@���@�  @���@���@�"�@�@��y@��H@��@���@���@���@��!@�M�@��T@�@���@�O�@��9@�bN@�(�@��@��@�|�@�dZ@�S�@�K�@�;d@�@�=q@�hs@��@��9@��@��@���@��u@�z�@�j@�bN@�Q�@�b@�|�@�l�@�o@���@�V@�@��7@�?}@���@��D@�bN@�(�@��P@�
=@�{@���@�?}@��`@��u@�1'@���@���@�C�@��@�E�@��@�@��h@�`B@�&�@���@��@��F@�\)@���@�ȴ@���@���@��+@�M�@�$�@���@��T@�@�G�@�V@���@�1'@���@�l�@�@��R@�
=@�o@��H@�5?@���@�`B@��@��@���@���@��D@�1'@�1@��
@���@���@��m@�1@�b@���@��w@���@���@���@�o@�=q@�-@�-@��@��@���@���@��@�p�@�X@��@�V@�%@���@���@�Ĝ@���@� �@��F@��P@��@���@��@��@�X@�&�@��`@��@�I�@��@���@��R@�n�@�ff@�ff@�V@���@�X@�?}@�&�@�V@��/@��@�Q�@� �@\)@~��@|�D@|�@{S�@z^5@y�#@x  @m?}@`�u@]�@U/@O\)@H�@?��@6��@/|�@(�u@%�h@ �u@z�@�u@��@�w@1@
�!@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ZA�Q�A�E�A�A�A�=qA�7LA�5?A�33A�1'A�5?A�33A�1'A�33A�/A�1'A�1'A�33A�5?A�33A�7LA�7LA�1'A�+A�+A�-A�-A�&�A�(�A�(�A�"�A�&�A�+A�/A�+A�(�A�-A�1'A�1'A�33A�5?A�5?A�/A��A���A�Q�A��A� �A��uA�E�A�1A�n�A���A��^A�ĜA�r�A�jA�1'A�%A��A�
=A��A��`A��9A}hsAv��ArZApM�Aj�Ad�!A`�A[�mAYAVM�AR�AO�hANA�AL��AL��AL��AK�FAJ��AH��ADn�A?
=A=��A<VA9�hA6ȴA5��A4�A2ĜA/�A.��A.I�A-�^A-�7A-A,=qA*��A)�A)p�A(��A'�mA&�yA&n�A%oA$=qA#�A#"�A!�FA JA/A��A&�A��A?}A��Az�AA�A1'A�TA\)AhsAn�A��A��A�jA�wA
=AoAx�A5?Az�A�jA�RAhsA��A�yA�A��A�+A-Al�A�\AhsA
ĜA
ȴA
�A	
=AA�A��At�AO�A\)A|�A�TA��A5?A�A ��A ��@��F@��@�hs@��/@�33@��@��@��@��@�b@��H@�z�@�@�33@��@�&�@�+@���@�~�@���@�%@�Ĝ@�r�@�1'@��@�ƨ@�S�@�@�~�@���@܃@۾w@�S�@ڧ�@�x�@֧�@�V@�t�@ѡ�@��y@��@�;d@�b@���@Ь@��@��@�V@�?}@ȼj@ȓu@�9X@�t�@�\)@�&�@���@ˍP@́@�=q@�"�@�b@�(�@��@�|�@�\)@�dZ@�ȴ@Ͳ-@�/@�bN@˅@�o@ʧ�@��@��@ȼj@ȃ@�1'@�Q�@Ǯ@�dZ@�\)@�S�@�C�@�"�@��@�v�@�/@��y@�V@�V@�M�@��@��^@��`@���@��y@��j@�;d@�=q@���@�Q�@��@���@�@�x�@���@�A�@�1'@�(�@� �@��m@�ƨ@�ȴ@�5?@�J@���@��#@��T@��#@��-@�x�@��^@��T@�J@�=q@��+@��R@���@��@�^5@��@�X@��u@��@��@��m@��m@�t�@��@�~�@�{@�@�{@���@��@�Z@� �@���@�K�@�
=@�n�@��@�@�@��h@�/@���@�  @���@���@�"�@�@��y@��H@��@���@���@���@��!@�M�@��T@�@���@�O�@��9@�bN@�(�@��@��@�|�@�dZ@�S�@�K�@�;d@�@�=q@�hs@��@��9@��@��@���@��u@�z�@�j@�bN@�Q�@�b@�|�@�l�@�o@���@�V@�@��7@�?}@���@��D@�bN@�(�@��P@�
=@�{@���@�?}@��`@��u@�1'@���@���@�C�@��@�E�@��@�@��h@�`B@�&�@���@��@��F@�\)@���@�ȴ@���@���@��+@�M�@�$�@���@��T@�@�G�@�V@���@�1'@���@�l�@�@��R@�
=@�o@��H@�5?@���@�`B@��@��@���@���@��D@�1'@�1@��
@���@���@��m@�1@�b@���@��w@���@���@���@�o@�=q@�-@�-@��@��@���@���@��@�p�@�X@��@�V@�%@���@���@�Ĝ@���@� �@��F@��P@��@���@��@��@�X@�&�@��`@��@�I�@��@���@��R@�n�@�ff@�ff@�V@���@�X@�?}@�&�@�V@��/@��@�Q�@� �@\)@~��@|�D@|�@{S�@z^5G�O�@x  @m?}@`�u@]�@U/@O\)@H�@?��@6��@/|�@(�u@%�h@ �u@z�@�u@��@�w@1@
�!@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	u�B	s�B	t�B	t�B	t�B	t�B	t�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	s�B	r�B	r�B	s�B	r�B	r�B	r�B	r�B	s�B	s�B	t�B	t�B	u�B	v�B	{�B	�=B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�\B	�B	|�B	w�B	q�B	iyB	bNB	YB	S�B	O�B	K�B	G�B	H�B	E�B	@�B	=qB	6FB	0!B	2-B	33B	;dB	G�B	F�B	=qB	7LB	1'B	�B	�B	�B	\B	bB	oB	bB	PB	
=B	1B	%B	B	B	B	B	B	1B	DB	PB	\B	hB	�B	uB	oB	�B	�B	�B	�B	hB	DB	DB		7B	JB	�B	�B	�B	�B	�B	#�B	+B	6FB	/B	"�B	�B	"�B	%�B	'�B	 �B	�B	@�B	O�B	W
B	[#B	O�B	P�B	`BB	o�B	q�B	q�B	p�B	k�B	e`B	\)B	W
B	W
B	S�B	L�B	E�B	A�B	?}B	?}B	@�B	B�B	J�B	:^B	(�B	%�B	�B	�B	�B	bB	JB	B	B��B��B��B�B�B�B��B��B��B��B	B	%B	+B	+B		7B	VB	hB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	.B	/B	0!B	-B	'�B	%�B	"�B	�B	 �B	$�B	(�B	49B	7LB	?}B	8RB	,B	&�B	/B	1'B	33B	49B	7LB	G�B	aHB	hsB	y�B	�1B	�PB	�{B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�3B	�-B	�'B	�!B	�!B	�B	�!B	�!B	�-B	�RB	�XB	�XB	�RB	�RB	�RB	�RB	�RB	�LB	�3B	�B	�'B	�?B	�^B	�^B	�^B	�LB	�3B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�qB	��B	B	ŢB	ȴB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�5B	�;B	�/B	�)B	�#B	�5B	�TB	�NB	�TB	�NB	�TB	�TB	�`B	�`B	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
PB
PB
PB
JB
JB
JB
JB
VB
hB
oB
oB
hB
bB
\B
bB
bB
bB
bB
hB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
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
&�B
%�B
%�B
%�B
)�B
&�B
.B
6FB
=qB
?}B
E�B
J�B
O�B
W
B
^5B
e`B
k�B
o�B
r�B
t�B
x�B
{�B
�B
�B
�B
�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	u�B	s�B	t�B	t�B	t�B	t�B	t�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	s�B	r�B	r�B	s�B	r�B	r�B	r�B	r�B	s�B	s�B	t�B	t�B	u�B	v�B	{�B	�$B	��B	��B	�iB	�kB	�vB	��B	��B	��B	�~B	�<B	�<B	��B	|�B	w�B	q�B	iVB	b-B	X�B	S�B	O�B	K�B	G�B	H�B	E�B	@aB	=MB	6"B	/�B	2
B	3B	;?B	G�B	F�B	=LB	7'B	1B	�B	hB	aB	7B	>B	GB	;B	(B	
B	B	�B	�B	�B	�B	 �B	�B	
B	B	(B	7B	BB	ZB	MB	FB	jB	tB	�B	kB	@B	B	B		B	"B	�B	vB	^B	vB	�B	#�B	*�B	6B	.�B	"�B	�B	"�B	%�B	'�B	 �B	�B	@WB	O�B	V�B	Z�B	O�B	P�B	`B	opB	q}B	qB	pxB	kYB	e2B	[�B	V�B	V�B	S�B	L�B	EwB	A`B	?PB	?RB	@XB	BcB	J�B	:4B	(�B	%�B	�B	�B	aB	8B	B	�B	 �B��B��B��B�B�sB�nB��B��B��B��B	�B	�B	�B	 B		B	'B	=B	UB	XB	_B	dB	sB	yB	�B	#�B	%�B	-�B	.�B	/�B	,�B	'�B	%�B	"�B	�B	 �B	$�B	(�B	4	B	7B	?NB	8%B	+�B	&�B	.�B	0�B	3B	4
B	7B	G�B	aB	hDB	y�B	�B	�B	�IB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�$B	� B	� B	�B	�B	�B	�B	�B	��B	��B	�	B	�,B	�+B	�+B	�B	��B	��B	��B	��B	��B	��B	�sB	�`B	�XB	�[B	�fB	�yB	�yB	�{B	�zB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�<B	�MB	�]B	�nB	ȀB	̗B	ΤB	ΤB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�+B	�)B	�7B	�8B	�?B	�DB	�DB	�EB	�HB	�NB	�OB	�MB	�OB	�TB	�VB	�YB	�]B	�\B	�aB	�bB	�bB	�hB	�mB	�mB	�mB	�sB	�B	�~B	�B	�B	�B	�B	�B	�}B	�B	�B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
	 B

B

B

B

B
B
B
B
B
B
B
B
B
1B
6B
6B
/B
,B
&B
+B
)B
)B
)B
/B
6B
6B
5B
?B
?B
BB
LB
VB
WB
[B
iB
hB
bB
^B
RB
UB
TB
SB
UB
[B
\B
aB
cB
nB
qB
yB
}B
}B
xB
~B
B
yB
zB
sB
sB
oB
oB
mB
zB
sB
tB
sB
�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
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
&�B
%�B
%�B
%�G�O�B
&�B
-�B
6B
=6B
?CB
EhB
J�B
O�B
V�B
]�B
e(B
kKB
ocB
ruB
t�B
x�B
{�B
��B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451262016080714512620160807145126  AO  ARCAADJP                                                                    20160123031627    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160123031627  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160123031627  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145126  IP                  G�O�G�O�G�O�                