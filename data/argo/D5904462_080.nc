CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-09T19:23:51Z AOML 3.0 creation; 2016-08-07T21:51:22Z UW 3.1 conversion     
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
resolution        =���   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20151009192351  20160807145122  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               PA   AO  5287_9017_080                   2C  D   APEX                            6529                            072314                          846 @�u�>cg�1   @�u����@0)�^5?}�d�Ƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    PA   B   B   @9��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BG��BP  BX  B`  BhffBpffBw33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  DuFfDy��D� D�I�D�y�D��3D��D�L�D��3D��fD� D�Y�D���D��fD�3D�33Dڐ D���D�fD�C3D�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@W�@�
=@�
=A�A%�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB2G�B9�HBA�HBIz�BQ�HBY�HBa�HBjG�BrG�By{B��B��B��B��B�#�B��B��B��B��B��B��B��B��B�#�B��qB��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDudzDy��D�
D�X�D���D��=D�(�D�[�D��=D��pD�
D�h�D���D��pD�"=D�B=Dڟ
D���D�pD�R=D�D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�XA�\)A�VA�VA�^5A�bNA�ffA�hsA�n�A�p�A�p�A�n�A�n�A�n�A�hsA�hsA�hsA�^5A�VA�O�A�E�A�O�A�bNA�PA�v�A⛦A�A�  A�bNAߍPA۟�A��A�1'A��/AӇ+Aҝ�Aљ�Aϝ�A��AΧ�A��HA�JAə�A�A��A��`A��`A�XA��
A�7LA��A��RA���A�\)A�`BA�Q�A�5?A�l�A� �A�I�A�;dA�1'A��A�l�A�ZA���A��-A�%A�^5A��DA�VA��mA��\A��RA�`BA���A�9XA�
=A�A�/A�1A��A�r�A��DA��A��A��!A���A�C�A�JA��A�A�A��A���A��9A�E�A�oA�$�A�7LA��;A�A�A~Q�A{�FA{S�Az-As�hAo\)Al{Aj�Af�Ad�9Ac?}Ab��A_�A[K�AV�yASx�AP$�AM�AI��AF��AE��AC��A?\)A<5?A:M�A:A9�;A7ƨA4�A3A2$�A1�mA0��A/
=A-"�A+��A*ffA)��A(��A(1A'�A&��A%"�A$(�A#��A#hsA"�9A"A�^A�AdZAM�A�/AI�A �A�#AA��AffAAC�A�A��A�A��AZA��A5?A~�AC�A�Ap�A�FAC�AffA�hA�`AhsA	�A	�A��AQ�A��A	G�A�PA��Az�AI�A��A�A1A ��A Q�@��m@�33@���@�&�@�bN@��@��@��@�G�@���@��@��y@��!@���@�V@��+@�dZ@���@�`B@��y@�@��@�O�@�w@�
=@�;d@���@�S�@�+@���@��@�@��@�&�@��@�1@��@�o@���@߮@�E�@�1@��#@�33@�ff@�M�@�?}@�+@�~�@ٺ^@�`B@�33@��@۝�@ە�@��
@�(�@�(�@���@�`B@�@�$�@ݩ�@�p�@�~�@�V@�hs@�Z@ו�@��@��@պ^@Լj@�dZ@��y@ҟ�@ҏ\@�E�@�{@��`@�(�@�|�@Ϯ@�A�@��;@�t�@υ@�;d@�=q@�@�X@˕�@���@ʗ�@���@�hs@�%@�(�@�ƨ@�\)@�S�@�;d@Ə\@�^5@�5?@š�@�X@�7L@���@ě�@�bN@�I�@�1'@þw@�@�n�@��@��7@��@�x�@�X@�hs@���@�{@���@���@��-@��@�ƨ@��@��+@��^@��@��@�hs@�O�@��@��j@���@���@���@��@���@�l�@�"�@��T@�`B@�G�@���@�Ĝ@��@��9@���@���@��y@���@�M�@�5?@�{@�@���@�hs@�7L@���@�Q�@�  @�ƨ@���@�+@�o@��H@��R@�5?@�hs@��`@��@�Q�@��;@�\)@�;d@�33@�o@��@��!@�@��h@�G�@�7L@�7L@�G�@��h@���@��@���@��j@���@�z�@�I�@� �@���@��@�o@�~�@���@���@���@�`B@���@�z�@�9X@���@���@��H@���@�E�@�$�@�$�@��@�{@��@���@���@�r�@�9X@���@�
=@�=q@��#@��^@��7@��`@�1'@���@�\)@�o@��y@���@���@�~�@�ff@�E�@�{@��@��#@���@�7L@��`@�z�@� �@��m@�ƨ@��P@�C�@��@���@��\@�n�@�V@�{@���@��@�`B@�X@�X@�&�@��@��j@�1@�|�@�K�@��@��y@���@�n�@��#@��-@�hs@���@�j@�A�@�(�@�ƨ@�
=@�E�@��#@�x�@�G�@��/@�Z@���@��w@���@��P@�t�@�@�M�@��u@���@���@}�h@r-@g;d@`�`@V5?@M��@Fff@;�m@4�D@.E�@(�@#��@�@�\@�y@�F@K�@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�K�A�XA�\)A�VA�VA�^5A�bNA�ffA�hsA�n�A�p�A�p�A�n�A�n�A�n�A�hsA�hsA�hsA�^5A�VA�O�A�E�A�O�A�bNA�PA�v�A⛦A�A�  A�bNAߍPA۟�A��A�1'A��/AӇ+Aҝ�Aљ�Aϝ�A��AΧ�A��HA�JAə�A�A��A��`A��`A�XA��
A�7LA��A��RA���A�\)A�`BA�Q�A�5?A�l�A� �A�I�A�;dA�1'A��A�l�A�ZA���A��-A�%A�^5A��DA�VA��mA��\A��RA�`BA���A�9XA�
=A�A�/A�1A��A�r�A��DA��A��A��!A���A�C�A�JA��A�A�A��A���A��9A�E�A�oA�$�A�7LA��;A�A�A~Q�A{�FA{S�Az-As�hAo\)Al{Aj�Af�Ad�9Ac?}Ab��A_�A[K�AV�yASx�AP$�AM�AI��AF��AE��AC��A?\)A<5?A:M�A:A9�;A7ƨA4�A3A2$�A1�mA0��A/
=A-"�A+��A*ffA)��A(��A(1A'�A&��A%"�A$(�A#��A#hsA"�9A"A�^A�AdZAM�A�/AI�A �A�#AA��AffAAC�A�A��A�A��AZA��A5?A~�AC�A�Ap�A�FAC�AffA�hA�`AhsA	�A	�A��AQ�A��A	G�A�PA��Az�AI�A��A�A1A ��A Q�@��m@�33@���@�&�@�bN@��@��@��@�G�@���@��@��y@��!@���@�V@��+@�dZ@���@�`B@��y@�@��@�O�@�w@�
=@�;d@���@�S�@�+@���@��@�@��@�&�@��@�1@��@�o@���@߮@�E�@�1@��#@�33@�ff@�M�@�?}@�+@�~�@ٺ^@�`B@�33@��@۝�@ە�@��
@�(�@�(�@���@�`B@�@�$�@ݩ�@�p�@�~�@�V@�hs@�Z@ו�@��@��@պ^@Լj@�dZ@��y@ҟ�@ҏ\@�E�@�{@��`@�(�@�|�@Ϯ@�A�@��;@�t�@υ@�;d@�=q@�@�X@˕�@���@ʗ�@���@�hs@�%@�(�@�ƨ@�\)@�S�@�;d@Ə\@�^5@�5?@š�@�X@�7L@���@ě�@�bN@�I�@�1'@þw@�@�n�@��@��7@��@�x�@�X@�hs@���@�{@���@���@��-@��@�ƨ@��@��+@��^@��@��@�hs@�O�@��@��j@���@���@���@��@���@�l�@�"�@��T@�`B@�G�@���@�Ĝ@��@��9@���@���@��y@���@�M�@�5?@�{@�@���@�hs@�7L@���@�Q�@�  @�ƨ@���@�+@�o@��H@��R@�5?@�hs@��`@��@�Q�@��;@�\)@�;d@�33@�o@��@��!@�@��h@�G�@�7L@�7L@�G�@��h@���@��@���@��j@���@�z�@�I�@� �@���@��@�o@�~�@���@���@���@�`B@���@�z�@�9X@���@���@��H@���@�E�@�$�@�$�@��@�{@��@���@���@�r�@�9X@���@�
=@�=q@��#@��^@��7@��`@�1'@���@�\)@�o@��y@���@���@�~�@�ff@�E�@�{@��@��#@���@�7L@��`@�z�@� �@��m@�ƨ@��P@�C�@��@���@��\@�n�@�V@�{@���@��@�`B@�X@�X@�&�@��@��j@�1@�|�@�K�@��@��y@���@�n�@��#@��-@�hs@���@�j@�A�@�(�@�ƨ@�
=@�E�@��#@�x�@�G�@��/@�Z@���@��w@���@��P@�t�@�G�O�@��u@���@���@}�h@r-@g;d@`�`@V5?@M��@Fff@;�m@4�D@.E�@(�@#��@�@�\@�y@�F@K�@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
�JB
��B
��B
�!B
�LB
�qB
�BDB(�B?}BcTBy�B�\B�JB�VB�{B�BBƨB�mBbB6FBYBiyBt�Bm�Bq�Bx�Bu�B�B��BBB�DBv�Bo�BgmBbNB_;BZBQ�BI�BC�B:^B)�B!�B�BB��B�B�;B�
BĜB�9B��B�JBz�BffBT�BE�B%�B{B
��B
�B
�
B
B
�B
�B
��B
��B
�uB
u�B
l�B
gmB
YB
VB
K�B
6FB
"�B
�B
�B	�B	ɺB	�!B	��B	�=B	x�B	n�B	gmB	S�B	>wB	'�B	�B	DB	B�B�yB�mB�BB��B��B��B��BɺBŢB��B�jB�^B�XB�FB�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�{B��B��B��B��B�{B�oB�bB�bB�bB��B��B��B��B��B��B��B�B�9B�FB�-B�-B�qB��B��B�B��BǮB��B�wB�jB�qB��B�NB�NB�B�B��B��B��B��B��B��B	B	B	  B��B��B	B	B	B	B	B	1B	VB	oB	�B	�B	)�B	7LB	@�B	<jB	8RB	:^B	=qB	<jB	;dB	=qB	A�B	?}B	8RB	8RB	K�B	cTB	hsB	hsB	dZB	[#B	R�B	K�B	F�B	A�B	>wB	?}B	M�B	ZB	cTB	bNB	Q�B	M�B	F�B	G�B	H�B	L�B	YB	^5B	]/B	]/B	bNB	gmB	k�B	p�B	w�B	|�B	�B	� B	�B	�VB	�=B	�B	�B	�B	�B	�B	�B	�B	�B	�DB	�JB	�\B	�\B	�\B	�uB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�-B	�9B	�?B	�?B	�?B	�FB	�RB	�XB	�jB	��B	B	B	B	ÖB	B	ÖB	ĜB	ÖB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�TB	�TB	�TB	�TB	�ZB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
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
	7B
	7B

=B

=B
DB
JB
PB
PB
VB
bB
bB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
{B
�B
�B
�B
(�B
1'B
6FB
=qB
C�B
I�B
Q�B
YB
_;B
e`B
iyB
m�B
q�B
u�B
x�B
|�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
�-B
�sB
��B
�B
�+B
�PB
�BB(�B?YBc1By�B�6B�#B�2B�UB��B�kBƁB�GB:B6 BX�BiRBt�BmjBq�Bx�Bu�B��B��B�hB�iB�Bv�BouBgEBb&B_BY�BQ�BI�BClB:4B)�B!�BcB�B��B�lB�B��B�oB�B��B�!Bz�Bf:BT�BEtB%�BNB
��B
�wB
��B
�dB
��B
��B
��B
��B
�JB
u�B
l`B
gAB
X�B
U�B
K�B
6B
"�B
�B
cB	�^B	ɓB	��B	��B	�B	x�B	nsB	gJB	S�B	>QB	'�B	kB	 B	 �B�B�WB�IB�"B��BϿBͮB̫BɘBŁB�`B�GB�=B�5B�#B�B�
B��B��B��B��B��B��B��B��B��B��B�yB�rB�tB�ZB�RB�[B�qB�eB�jB�dB�YB�NB�@B�BB�=B�dB�kB�xB��B��B��B��B��B�B�!B�
B�B�KB̧BβB��BϸBǆB�\B�PB�DB�JB��B�)B�)B�]B�dB��B��B��B��B��B��B	�B	�B��B��B��B	�B	�B	�B	�B	�B	B	+B	CB	|B	�B	)�B	7B	@VB	<>B	8%B	:2B	=CB	<:B	;:B	=DB	A[B	?RB	8$B	8'B	K�B	c&B	hFB	hGB	d+B	Z�B	R�B	K�B	F{B	A[B	>HB	?PB	M�B	Y�B	c"B	b"B	Q�B	M�B	F{B	G~B	H�B	L�B	X�B	^B	] B	\�B	bB	g>B	kWB	ptB	w�B	|�B	��B	�B	��B	�#B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�+B	�*B	�CB	�KB	�EB	�TB	��B	��B	��B	��B	��B	��B	��B	�B	�gB	�aB	�hB	�aB	�jB	�gB	�_B	�_B	�nB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�
B	�B	�B	�$B	�7B	�QB	�]B	�]B	�[B	�dB	�^B	�`B	�iB	�`B	�yB	ɉB	ɇB	ʍB	˓B	˓B	̘B	͞B	ΧB	ϪB	бB	ϫB	ΥB	̘B	бB	ҾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�
B	� B	� B	�B	�B	�&B	�)B	�>B	�]B	�iB	�mB	�sB	�tB	�vB	�xB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
	B
	B

B

B
B
B
B
B
 B
*B
+B
7B
6B
9B
=B
CB
DB
DB
JB
OB
PB
MB
MB
RB
OB
PB
PB
OB
OB
OB
PB
QG�O�B
DB
ZB
bB
�B
(�B
0�B
6B
=8B
C^B
I�B
Q�B
X�B
^�B
e(B
i@B
mXB
qqB
u�B
x�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451222016080714512220160807145122  AO  ARCAADJP                                                                    20151009192351    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151009192351  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151009192351  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145122  IP                  G�O�G�O�G�O�                