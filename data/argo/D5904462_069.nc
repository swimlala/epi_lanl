CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-12T09:16:20Z AOML 3.0 creation; 2016-08-07T21:51:20Z UW 3.1 conversion     
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
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150812091620  20160807145120  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               EA   AO  5287_9017_069                   2C  D   APEX                            6529                            072314                          846 @�g���1   @�g����@1Z�1�d�� ě�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    EA   B   B   @333@y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D�fD�FfD�y�D��fD�3D�<�D�� D���D�fD�FfD��3D��3D���D�I�DچfD�ɚD��D�I�D�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @QG�@��
@��
A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBjG�Bq�HByz�B��B��B��B��B��B��B��B��B��B�#�B�#�B��B��qB��B��B��B��B��B��B�#�B�W
BԽqB��B��B��B��B��B��B�#�B��B��B��C ^�CxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC ��C"��C$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�zDKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt��Dy�GD�%pD�UpD���D��pD�"=D�K�D��
D���D�pD�UpD��=D��=D��D�X�DڕpD�ؤD��D�X�D�=D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�  A�
=A�1A�1A�%A�A���A���A��A��
A�wA�A�hA�+A�bNA� �A�^A❲A�A�r�A�^5A� �A�ĜA�ȴAߺ^Aݴ9A��A��A�v�A��A���AבhA�9XA�1'A��TA�ZA��A�7LA�M�AӮA�ȴA�G�A�^5A��`AмjA�v�A���A΍PA�`BA˺^A�x�AȑhA���A��
Að!A��HA�5?A���A��-A�|�A���A�-A��TA��A�ffA��A���A��+A�(�A���A��hA�;dA���A��A�9XA��HA���A���A��A��A� �A��RA��A��\A��9A��-A�t�A�  A�~�A���A��A��!A�  A��PA�x�A�A�A�A��mA�%A���A��A���A�K�A���A���A���A�;dA���A�A|��AxȴArffAm��AjbA`��AZ1'AX��AW+AV�uAVVAU��AR�!AR�AQ�PAP��AOp�AMO�AJ��AJbAG��AE�AB��ABv�AA%A?hsA=;dA;?}A97LA6��A4��A3�#A333A1�A.�yA-hsA-�A+C�A)�A)�hA(1A&z�A&-A%�-A%oA$VA#�A"��A!�^A!l�A ��A �RA ffAx�A��An�A��A��A��A�PA��AI�Az�AI�A��AO�A�AQ�A1A�AƨA�A�
A�`A��A�A�hA
E�A	7LA��A��A��A	�A��A��A��A�uA$�A�AdZA33A�jAbNA�A�TA�hAXA��A��A��Ax�AoA �yA �9A ZA b@��@�{@���@��
@���@���@�J@��T@�G�@��@�Ĝ@��D@� �@���@��@�C�@�
=@���@�-@���@�/@�z�@�w@�ȴ@�v�@�$�@�G�@@��@���@�~�@�V@��@�Ĝ@�r�@�Z@땁@�@��@陚@�Ĝ@�9X@�(�@��m@�l�@��H@�+@��@��@�@�(�@��m@�P@�o@��@��@�ff@�@��@��#@�-@�7@��@�j@�Q�@�1@�C�@��y@���@�v�@���@�`B@���@܋D@�bN@�(�@��@�l�@��@��H@ٺ^@���@׍P@�C�@�@�E�@ղ-@�hs@�&�@�r�@�1@��;@�K�@�@��@ҟ�@�ff@��@��@���@�I�@�K�@Χ�@�^5@Ͳ-@�%@̓u@�Q�@��@���@�ƨ@�l�@�+@��H@�n�@��T@�/@�Q�@��@�C�@Ə\@���@Ł@��@Ĵ9@ļj@ļj@ģ�@ě�@�j@���@�+@�ȴ@�M�@���@�?}@��/@�j@�t�@�n�@�-@��#@��-@��h@�x�@�X@���@��@��w@���@��@�l�@�;d@��@��+@��#@�x�@�hs@�7L@�Z@��
@�K�@�M�@���@�hs@�%@�z�@�(�@� �@��w@��@���@�E�@�@��-@��@�Ĝ@�Z@�9X@� �@��@�"�@�~�@�E�@��@��@���@��h@�G�@��`@�1@���@�|�@�t�@�l�@�33@��@��@��R@�M�@�J@��@���@�?}@���@��u@�Q�@��w@��@�C�@�
=@��y@��R@��+@�ff@�V@���@�x�@�7L@�%@���@�j@�A�@��@�  @��
@���@�
=@��!@���@�@��@�X@�/@�/@�&�@��@���@��9@��9@��@��@�bN@�1'@�z�@��D@��;@�v�@�=q@�@���@���@�p�@�?}@�?}@�/@�/@�/@�/@�/@�&�@���@�z�@��m@�@��R@�~�@�=q@�@�@�x�@�O�@���@�bN@�bN@���@��^@��@}/@q�7@g+@]�-@T1@K�F@C�
@<�j@6�@1��@+"�@&ȴ@"-@��@A�@33@5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�%A�  A�
=A�1A�1A�%A�A���A���A��A��
A�wA�A�hA�+A�bNA� �A�^A❲A�A�r�A�^5A� �A�ĜA�ȴAߺ^Aݴ9A��A��A�v�A��A���AבhA�9XA�1'A��TA�ZA��A�7LA�M�AӮA�ȴA�G�A�^5A��`AмjA�v�A���A΍PA�`BA˺^A�x�AȑhA���A��
Að!A��HA�5?A���A��-A�|�A���A�-A��TA��A�ffA��A���A��+A�(�A���A��hA�;dA���A��A�9XA��HA���A���A��A��A� �A��RA��A��\A��9A��-A�t�A�  A�~�A���A��A��!A�  A��PA�x�A�A�A�A��mA�%A���A��A���A�K�A���A���A���A�;dA���A�A|��AxȴArffAm��AjbA`��AZ1'AX��AW+AV�uAVVAU��AR�!AR�AQ�PAP��AOp�AMO�AJ��AJbAG��AE�AB��ABv�AA%A?hsA=;dA;?}A97LA6��A4��A3�#A333A1�A.�yA-hsA-�A+C�A)�A)�hA(1A&z�A&-A%�-A%oA$VA#�A"��A!�^A!l�A ��A �RA ffAx�A��An�A��A��A��A�PA��AI�Az�AI�A��AO�A�AQ�A1A�AƨA�A�
A�`A��A�A�hA
E�A	7LA��A��A��A	�A��A��A��A�uA$�A�AdZA33A�jAbNA�A�TA�hAXA��A��A��Ax�AoA �yA �9A ZA b@��@�{@���@��
@���@���@�J@��T@�G�@��@�Ĝ@��D@� �@���@��@�C�@�
=@���@�-@���@�/@�z�@�w@�ȴ@�v�@�$�@�G�@@��@���@�~�@�V@��@�Ĝ@�r�@�Z@땁@�@��@陚@�Ĝ@�9X@�(�@��m@�l�@��H@�+@��@��@�@�(�@��m@�P@�o@��@��@�ff@�@��@��#@�-@�7@��@�j@�Q�@�1@�C�@��y@���@�v�@���@�`B@���@܋D@�bN@�(�@��@�l�@��@��H@ٺ^@���@׍P@�C�@�@�E�@ղ-@�hs@�&�@�r�@�1@��;@�K�@�@��@ҟ�@�ff@��@��@���@�I�@�K�@Χ�@�^5@Ͳ-@�%@̓u@�Q�@��@���@�ƨ@�l�@�+@��H@�n�@��T@�/@�Q�@��@�C�@Ə\@���@Ł@��@Ĵ9@ļj@ļj@ģ�@ě�@�j@���@�+@�ȴ@�M�@���@�?}@��/@�j@�t�@�n�@�-@��#@��-@��h@�x�@�X@���@��@��w@���@��@�l�@�;d@��@��+@��#@�x�@�hs@�7L@�Z@��
@�K�@�M�@���@�hs@�%@�z�@�(�@� �@��w@��@���@�E�@�@��-@��@�Ĝ@�Z@�9X@� �@��@�"�@�~�@�E�@��@��@���@��h@�G�@��`@�1@���@�|�@�t�@�l�@�33@��@��@��R@�M�@�J@��@���@�?}@���@��u@�Q�@��w@��@�C�@�
=@��y@��R@��+@�ff@�V@���@�x�@�7L@�%@���@�j@�A�@��@�  @��
@���@�
=@��!@���@�@��@�X@�/@�/@�&�@��@���@��9@��9@��@��@�bN@�1'@�z�@��D@��;@�v�@�=q@�@���@���@�p�@�?}@�?}@�/@�/@�/@�/@�/@�&�@���@�z�@��m@�@��R@�~�@�=q@�@�@�x�@�O�@���G�O�@�bN@���@��^@��@}/@q�7@g+@]�-@T1@K�F@C�
@<�j@6�@1��@+"�@&ȴ@"-@��@A�@33@5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
R�B
S�B
R�B
S�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
YB
bNB
q�B
�%B
�+B
�%B
�+B
�B
�B
� B
v�B
p�B
k�B
iyB
p�B
q�B
s�B
�%B
��B
�B
�B
�}B
��B
�B
�B
=B(�B"�B(�BC�B�bB�5B��B#�B/BcTB�=B�JB~�B�\B�uB�bB��B��B�B{�Bp�Bm�BjBhsBcTBQ�BI�BA�B�dB�B��BĜB�wB�B��B��B��Bx�BJ�B	7B�B�B�Bm�BcTBQ�B6FB'�B��B��B��B��B��B�yB�B�B�VB�bB{�BH�B�B
��B
�ZB
��B
��B
�3B
��B
�1B
n�B
R�B
33B
DB	�
B	�B	�JB	N�B	"�B	,B	8RB	A�B	@�B	<jB	/B	-B	,B	'�B	!�B	�B	\B		7B��B��B��B��B�B�B�TB�B��BȴBŢBŢBƨB��B��B��B��B��B��B��B�
B��B��B��B�B�NB�NB�fB�B�B�B�B�B�B�B��B��B�B�;B�/B�BB�/B�)B�HB�B��B��B��B��B	B	1B		7B	JB	VB	oB	�B	�B	�B	�B	#�B	7LB	E�B	S�B	T�B	T�B	Q�B	Q�B	R�B	T�B	XB	\)B	aHB	ffB	jB	l�B	n�B	m�B	k�B	n�B	{�B	�1B	�7B	�=B	�=B	�PB	�VB	�hB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�RB	�RB	�LB	�FB	�FB	�^B	�qB	�qB	�wB	B	ĜB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�
B	�B	�B	�
B	�B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�NB	�TB	�TB	�NB	�HB	�HB	�BB	�HB	�TB	�`B	�mB	�mB	�mB	�mB	�mB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B

=B

=B
1B
B
B
B
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
%B

=B

=B
1B
1B
+B
+B
+B
+B
1B
1B
+B
PB

=B
VB
�B
#�B
-B
0!B
8RB
?}B
D�B
J�B
P�B
VB
[#B
^5B
cTB
ffB
jB
m�B
s�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
R�B
S�B
R�B
S�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
U�B
X�B
b0B
q�B
�B
�B
�B
�B
��B
��B
�B
v�B
p�B
kgB
iZB
p�B
q�B
s�B
�B
��B
��B
��B
�ZB
̫B
��B
�B
B(�B"�B(�BCrB�=B�B��B#�B.�Bc0B�B�$B~�B�7B�OB�;B�uB�lB��B{�Bp|BmgBjVBhNBc)BQ�BI�BAbB�;B��BʖB�rB�QB��B��B�aB��Bx�BJ�B	
B�B��B�BmhBc(BQ�B6B'�B��B��B��B��B��B�KB��B��B�*B�4B{�BH�B�B
��B
�/B
ήB
�`B
�B
��B
�B
nlB
R�B
3B
B	��B	��B	�$B	N�B	"�B	+�B	8/B	AeB	@_B	<GB	.�B	,�B	+�B	'�B	!�B	wB	9B		B��B��B��B��B�B�jB�2B��BϼBȑBŀBŀBƆBʡBϺB��BϼB��B��B��B��B��B��B��B��B�(B�'B�CB�]B�^B�eB�dB�lB�yB�xB��B��B�dB�B�	B�B�B�B�!B�pB��B��B��B��B	�B		B		B	 B	.B	GB	rB	~B	�B	�B	#�B	7!B	EvB	S�B	T�B	T�B	Q�B	Q�B	R�B	T�B	W�B	[�B	aB	f7B	jSB	l[B	njB	meB	kWB	niB	{�B	�B	�B	�B	�B	�"B	�%B	�:B	�3B	�:B	�MB	�\B	�B	�B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�"B	�B	�B	�B	�,B	�?B	�>B	�FB	�]B	�jB	�rB	�tB	ȂB	ȁB	ʎB	̛B	̚B	͡B	ΨB	ϫB	ѹB	ѹB	ѸB	ҾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�B	�&B	�&B	�&B	�'B	�(B	�&B	�&B	�'B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�+B	�9B	�9B	�:B	�8B	�8B	�/B	�2B	�8B	�@B	�BB	�>B	�?B	�?B	�FB	�?B	�@B	�BB	�HB	�RB	�QB	�QB	�VB	�WB	�SB	�PB	�OB	�RB	�QB	�QB	�RB	�\B	�mB	�B	�B	�{B	�zB	�wB	�B	�B	�zB	�{B	�yB	�{B	�{B	�zB	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�uB	�sB	�sB	�wB	�yB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B

B

B
�B
�B
�B
�B
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
�B

B

B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B

B
B
kB
#�B
,�B
/�B
8B
?FB
DcB
J�B
P�B
U�B
Z�B
]�B
cB
f/B
jGB
mYB
sB
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451202016080714512020160807145120  AO  ARCAADJP                                                                    20150812091620    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150812091620  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150812091620  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145120  IP                  G�O�G�O�G�O�                