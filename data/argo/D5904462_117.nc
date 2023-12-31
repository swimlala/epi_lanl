CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-22T19:36:16Z AOML 3.0 creation; 2016-08-07T21:51:28Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160422193616  20160807145128  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               uA   AO  5287_9017_117                   2C  D   APEX                            6529                            072314                          846 @צ_�1]r1   @צ`���@0�p��
=�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    uA   B   B   @&ff@�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D� D�<�D���D���D�	�D�6fD���D���D� D�` D��3D��fD�3D�<�Dډ�D��3D��D�I�D�c3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @;�@�@ʏ\AG�A%G�AEG�AeG�A���A��
A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B��)B��)B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�B��)B��)B�u�B�u�B��B��B��B��B���B���B���C T{CT{CnCT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CnCT{CT{C T{C"T{C$T{C&:�C(:�C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD��DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=��D>D>�D?D?�D@D@�DADA�DBDB��DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\��D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dy�RD��D�G\D��)D��\D�)D�@�D��\D��\D��D�j�D���D���D��D�G\Dڔ)D���D�\D�T)D�m�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AѶFAѴ9A�AѸRA���A�A�A���A�A�A�ƨA�ƨA�ȴA���A���A���A���A���A���A�A�ĜA�ƨA�ĜAѲ-Aѡ�A�\)A�-A�1A���AН�A�v�A�M�A�x�AξwA�5?A���AͰ!A͝�A�t�A�O�A� �A���A̲-A̅A�VA�$�A�A�A˅A�n�A���A�|�A�oA�K�A�ZA�1A��PA�ffA��PA���A�ĜA�
=A���A�|�A�(�A�oA��!A�VA��A�VA���A�+A���A�1'A�A�A��A��uA��yA���A�5?A��A�M�A��jA�`BA��!A���A�+A��A��`A��7A��A�%A�bA�E�A�A�z�A�^5A���A���A��A�O�A�VA��^A�1'A���A��TA�I�A���A�ȴA��A�K�A�%Az�HAxJAwdZAt-Ap�jAm�Ak�mAk33Ah�Af�\AeoAb�+A[hsAX��AV��AT�\AS�hAR�HAQ�;AP�AL=qAI�
AHM�AC��AAK�A@n�A?G�A>5?A<A�A:��A:��A:^5A9��A8�/A6$�A4�A2�uA1�#A0�!A/�#A-t�A+��A*��A*A�A*A�A*A�A*E�A*=qA*bA(A"�A�mA/A �A��A�-A�A��A(�A��An�AZA%A�jA��A�A�\A��A1AK�A
bNA
1A	XA�HA �A�A1A�9A1A��AC�A�AA�A�
A�A7LA��A�A �@���@�/@��j@��T@��`@� �@���@�C�@���@�M�@��@���@�O�@��/@�  @�+@�-@�b@�@���@�A�@�@�7@��@�^5@�X@��/@�bN@�1@�|�@◍@�{@��#@���@�O�@���@��y@���@ޗ�@�-@���@��#@ݩ�@݁@�G�@ܴ9@�r�@�I�@۶F@ڏ\@��@��#@���@׮@�K�@�@�V@���@Դ9@�S�@�
=@��H@���@Ұ!@ҟ�@҇+@�n�@�-@ёh@мj@���@��y@�@͑h@�p�@�G�@�%@�Ĝ@�bN@���@��H@��@�ƨ@�$�@ũ�@�hs@���@�A�@��@Õ�@�;d@�^5@��@��#@�x�@�%@��@�Z@�b@���@�-@��^@�x�@�7L@���@��D@�j@�I�@�  @��@��@�v�@��^@�O�@��/@� �@�C�@��@��@�ȴ@���@�v�@�5?@�{@��@���@�%@��
@�+@�
=@��@��H@��@���@��!@���@�@���@�Z@�Q�@�1'@��@�  @�ƨ@��@��@�S�@��@�~�@�=q@���@��-@�`B@���@�(�@��F@��P@�\)@�33@�"�@��@�o@�@��y@��R@�ff@��@��-@���@��h@�p�@���@���@�1'@��m@���@��@�C�@���@�ff@��-@��`@��u@�r�@�bN@�I�@�  @��P@���@�J@�7L@�Ĝ@��u@��@�bN@�Q�@�I�@�9X@�1@��;@��w@��@�S�@�33@�
=@���@��y@���@�~�@�-@��h@���@��@�Z@�A�@��@���@��;@�ƨ@��F@��F@��F@��F@���@�C�@��@��!@�=q@��@�hs@��@��9@�r�@�9X@�(�@��@�1@��w@�dZ@�;d@��@��!@�~�@�=q@�$�@�{@�J@�@��T@���@���@���@��7@�?}@��@�S�@�
=@���@�v�@��@�G�@��@��@��@���@��`@��`@���@�j@�  @�ƨ@��P@�S�@��@���@�ȴ@���@�^5@�M�@�-@��#@���@�p�@�X@�?}@���@��@�bN@���@��u@}��@vV@n{@cC�@[dZ@Q&�@I�#@>��@8Ĝ@1�#@,j@*=q@$j@  �@�F@�-@o@K�@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AѶFAѴ9A�AѸRA���A�A�A���A�A�A�ƨA�ƨA�ȴA���A���A���A���A���A���A�A�ĜA�ƨA�ĜAѲ-Aѡ�A�\)A�-A�1A���AН�A�v�A�M�A�x�AξwA�5?A���AͰ!A͝�A�t�A�O�A� �A���A̲-A̅A�VA�$�A�A�A˅A�n�A���A�|�A�oA�K�A�ZA�1A��PA�ffA��PA���A�ĜA�
=A���A�|�A�(�A�oA��!A�VA��A�VA���A�+A���A�1'A�A�A��A��uA��yA���A�5?A��A�M�A��jA�`BA��!A���A�+A��A��`A��7A��A�%A�bA�E�A�A�z�A�^5A���A���A��A�O�A�VA��^A�1'A���A��TA�I�A���A�ȴA��A�K�A�%Az�HAxJAwdZAt-Ap�jAm�Ak�mAk33Ah�Af�\AeoAb�+A[hsAX��AV��AT�\AS�hAR�HAQ�;AP�AL=qAI�
AHM�AC��AAK�A@n�A?G�A>5?A<A�A:��A:��A:^5A9��A8�/A6$�A4�A2�uA1�#A0�!A/�#A-t�A+��A*��A*A�A*A�A*A�A*E�A*=qA*bA(A"�A�mA/A �A��A�-A�A��A(�A��An�AZA%A�jA��A�A�\A��A1AK�A
bNA
1A	XA�HA �A�A1A�9A1A��AC�A�AA�A�
A�A7LA��A�A �@���@�/@��j@��T@��`@� �@���@�C�@���@�M�@��@���@�O�@��/@�  @�+@�-@�b@�@���@�A�@�@�7@��@�^5@�X@��/@�bN@�1@�|�@◍@�{@��#@���@�O�@���@��y@���@ޗ�@�-@���@��#@ݩ�@݁@�G�@ܴ9@�r�@�I�@۶F@ڏ\@��@��#@���@׮@�K�@�@�V@���@Դ9@�S�@�
=@��H@���@Ұ!@ҟ�@҇+@�n�@�-@ёh@мj@���@��y@�@͑h@�p�@�G�@�%@�Ĝ@�bN@���@��H@��@�ƨ@�$�@ũ�@�hs@���@�A�@��@Õ�@�;d@�^5@��@��#@�x�@�%@��@�Z@�b@���@�-@��^@�x�@�7L@���@��D@�j@�I�@�  @��@��@�v�@��^@�O�@��/@� �@�C�@��@��@�ȴ@���@�v�@�5?@�{@��@���@�%@��
@�+@�
=@��@��H@��@���@��!@���@�@���@�Z@�Q�@�1'@��@�  @�ƨ@��@��@�S�@��@�~�@�=q@���@��-@�`B@���@�(�@��F@��P@�\)@�33@�"�@��@�o@�@��y@��R@�ff@��@��-@���@��h@�p�@���@���@�1'@��m@���@��@�C�@���@�ff@��-@��`@��u@�r�@�bN@�I�@�  @��P@���@�J@�7L@�Ĝ@��u@��@�bN@�Q�@�I�@�9X@�1@��;@��w@��@�S�@�33@�
=@���@��y@���@�~�@�-@��h@���@��@�Z@�A�@��@���@��;@�ƨ@��F@��F@��F@��F@���@�C�@��@��!@�=q@��@�hs@��@��9@�r�@�9X@�(�@��@�1@��w@�dZ@�;d@��@��!@�~�@�=q@�$�@�{@�J@�@��T@���@���@���@��7@�?}@��@�S�@�
=@���@�v�@��@�G�@��@��@��@���@��`@��`@���@�j@�  @�ƨ@��P@�S�@��@���@�ȴ@���@�^5@�M�@�-@��#@���@�p�@�X@�?}@���@��G�O�@���@��u@}��@vV@n{@cC�@[dZ@Q&�@I�#@>��@8Ĝ@1�#@,j@*=q@$j@  �@�F@�-@o@K�@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B%BbBuBoBVBbB{B�B�B�B�B�B�B�B �B �B�B1'BS�BiyB� B��BɺB�B'�BM�Bz�Bw�B�1B�'B�RB�dB�jB�3B�=B�B�B�By�Bp�Bl�Bn�Br�Bk�B9XB#�B�BVBB�5B�RB��B�{B�uB�oB�7B|�Bk�B1'B#�B�BhB  B
�sB
��B
B
�^B
�'B
��B
��B
��B
��B
��B
�B
w�B
s�B
n�B
YB
6FB
$�B
�B
	7B	�B	�B	��B	ƨB	�XB	�B	��B	�hB	iyB	YB	L�B	C�B	=qB	8RB	0!B	$�B	�B	\B	+B��B��B��B�B�B�B�B�B�B�B�sB�ZB�TB�`B�ZB�ZB�/B��BŢBÖBBB��B��B��B�qB�XB�jB�qB�dB�RB�^B�dB�jB�jB�wB�qB��BǮB��B��B�
B�B�/B�/B�/B�;B�;B�5B�;B�yB�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	%B		7B	1B	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	(�B	33B	5?B	9XB	;dB	=qB	>wB	@�B	E�B	H�B	J�B	M�B	Q�B	XB	[#B	\)B	]/B	aHB	cTB	dZB	dZB	e`B	gmB	k�B	m�B	m�B	o�B	t�B	v�B	w�B	{�B	� B	�B	�B	�B	�B	�+B	�=B	�DB	�JB	�JB	�JB	�JB	�PB	�PB	�PB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�dB	�wB	�}B	��B	��B	��B	B	B	B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
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
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
PB
PB
VB
bB
hB
hB
hB
oB
uB
uB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
)�B
-B
2-B
9XB
>wB
F�B
K�B
S�B
XB
]/B
bNB
dZB
jB
m�B
q�B
v�B
y�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
��B
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
�B
�B
�B
�B
��B
�B
�B
�B
�B
�B
�~B
�yB
�rB
�sB
�B
�B
��B
��BBJB^BVB?BHB`BiB�B�B�B�B�B�B �B �B�B1BS�BiaB�B��BɠB�B'�BM�Bz�Bw�B�B�B�7B�HB�OB�B� B��B��B��By�Bp�BloBn|Br�BklB98B#�BrB7B�B�B�2B��B�[B�WB�QB�B|�BkjB1B#�B�BKB
��B
�VB
��B
�qB
�?B
�B
��B
��B
��B
��B
��B
��B
w�B
s�B
n{B
X�B
6,B
$�B
�B
	B	�B	��B	˭B	ƎB	�AB	��B	��B	�QB	ibB	YB	L�B	C�B	=[B	8>B	0B	$�B	sB	IB	B��B��B��B�B�B�B�B�B�B�rB�^B�HB�?B�LB�HB�EB�B˵BŏBÃB�~B�~B�vB�yB�oB�]B�DB�WB�]B�RB�?B�KB�PB�UB�WB�dB�[B�nBǘBʪB��B��B�B�B�B�B�$B�%B�B�$B�dB�gB�vB�zB�B�B��B��B��B��B��B��B��B��B��B��B	
B		B	B	CB	UB	aB	fB	fB	eB	lB	nB	oB	lB	qB	yB	yB	xB	�B	�B	"�B	#�B	%�B	(�B	3B	5%B	9;B	;GB	=TB	>[B	@hB	E�B	H�B	J�B	M�B	Q�B	W�B	[B	\B	]B	a+B	c8B	d=B	d=B	eCB	gNB	kfB	mtB	muB	o�B	t�B	v�B	w�B	{�B	�B	��B	��B	��B	��B	�
B	�B	�#B	�+B	�(B	�*B	�(B	�1B	�1B	�2B	�6B	�CB	�FB	�\B	�lB	�mB	�kB	�rB	�sB	�sB	�zB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�+B	�.B	�DB	�VB	�XB	�bB	�gB	�fB	�nB	�mB	�mB	�sB	�yB	ŁB	ƅB	ȐB	ɗB	əB	ʞB	̪B	ͳB	ͲB	ͯB	ͳB	ζB	ηB	θB	ηB	εB	ϺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�*B	�*B	�)B	�'B	�1B	�<B	�=B	�AB	�BB	�BB	�IB	�JB	�IB	�PB	�QB	�WB	�[B	�ZB	�]B	�ZB	�ZB	�\B	�cB	�fB	�gB	�gB	�oB	�lB	�vB	�xB	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
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
�B
�B
�B
B
B
B
B
	B
	B

B

B

B

B

B
!B
 B
 B
$B
%B
+B
,B
,B
)B
)B
+B
1B
1B
1B
+B
+B
/B
=B
BB
CB
AB
JB
PB
QB
QB
OB
QB
QB
PB
PB
WB
\B
\B
]B
cB
dB
`B
aB
bB
hB
iB
gB
hB
iB
nB
nB
iB
oB
oG�O�B
�B
"�B
)�B
,�B
2B
93B
>PB
F�B
K�B
S�B
W�B
]B
b)B
d4B
jWB
mlB
q�B
v�B
y�B
|�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451282016080714512820160807145128  AO  ARCAADJP                                                                    20160422193616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160422193616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160422193616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145128  IP                  G�O�G�O�G�O�                