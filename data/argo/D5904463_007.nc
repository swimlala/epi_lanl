CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:51Z AOML 3.0 creation; 2016-08-07T22:44:58Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221451  20160807154458  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_007                   2C  D   APEX                            6530                            072314                          846 @�酀 1   @��� @)���Q��co��-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B'��B/��B8  B@  BHffBN��BW��B`  Bh  BpffBzffB33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyffD��D�P D�P D��3D��fD�FfD�� D���D�3D�FfD��3Dǹ�D��D�@ D�i�D��3D� D�)�D�|�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�B�RBQ�B!Q�B(�B0�B9Q�BAQ�BI�RBP�BX�BaQ�BiQ�Bq�RB{�RB�B�B���B���B���B���B���B��)B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B��)B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{C:�CT{CT{CT{CT{C T{C"T{C$T{C&nC(T{C*T{C,T{C.T{C0:�C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt��Dy{�D�\D�Z�D�Z�D���D� �D�P�D���D��\D��D�P�D���D��)D��)D�J�D�t)D���D��D�4)D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A��A���A���A���A���A��A��A���A���AޮAީ�Aޣ�Aޝ�Aޕ�AލPAމ7AރA�z�AݶFA���A���A�oA�^5A�bA�l�A̡�A��A�(�A�+A�A�A���A�p�A��A���A��mA�"�A��PA���A��A��hA��\A�ȴA��A�ĜA�E�A��;A��A�VA��A���A�oA���A�  A��;A���A�I�A���A�jA�5?A�p�A�
=A�=qA�bNA��-A�A�A�S�A���A�bNAy�Aq�Al�AfffAa"�A]�A[|�AZjAX�AS��ARr�AN�AJM�AF�HAC�AA;dA<�yA9O�A8JA7�wA7��A7/A69XA6bNA6�A6�A733A7�;A7��A7�#A7ƨA7��A6��A4�DA1l�A1G�A2ĜA4�A4�A3�A2�A2�A2�uA2bNA1�A.�A,VA+�hA*��A)�mA)�
A)x�A)\)A(A�A'oA&~�A&$�A%�A#��A"�A"ĜA"�`A"��A!�^A!XA!"�A ^5A �A�;A��A��Ax�AhsA33A��A��A1'A  A��A�A33A�HA�9AM�A�A�uA5?AI�AA�A33A��A�-A33A�!A(�A�wA��A\)A�A��A��A^5A(�A�TA�hAdZA?}AA�uAE�A$�A�A�#A�^A�hA�;A�FA�^A��Ap�A�A�7A��A��A
=A��AjA �AbA��A�mAp�AG�A7LAVA�HA��AI�AJA�#AAXA�jAffA�;AG�A
�yA
��A
v�A
-A
A	��A�HAbA��AdZAoA��A�uAQ�A-A�
AO�A�/A�jAQ�A=qA�A�FAdZA��A��Az�AE�A  A�A�A ��A $�@�^5@�/@�Ĝ@�z�@�Q�@��@��F@��H@���@�-@��@��@�b@��@�+@���@�7L@��/@��;@�l�@�\@�v�@�n�@�@�X@�%@�j@�"�@�\@�M�@��@�`B@��`@�Z@�@��y@�^5@��@���@��@�p�@�j@�l�@���@���@�I�@���@��
@�t�@���@�E�@�G�@�9X@ߥ�@��@ޗ�@�=q@���@���@�hs@܋D@���@�l�@�+@��@�^5@ّh@��@��@�(�@ם�@�dZ@��@֏\@�J@�G�@���@ԋD@���@�+@҇+@���@ѩ�@�`B@Ѓ@��m@�K�@Ώ\@���@̋D@��m@ʇ+@�@��T@�p�@�j@���@ǥ�@�S�@�o@��y@��@őh@��@ēu@�1'@��;@öF@�|�@�|�@�l�@�M�@��h@�hs@�V@���@� �@��F@���@�n�@�E�@�J@�7L@��D@� �@�|�@�+@���@���@���@��^@�`B@��@�I�@��
@��P@�;d@��@��R@�=q@��@�`B@�7L@��/@�I�@��@�33@��@��+@�-@��#@��^@���@���@���@��h@�?}@��@���@�(�@��@��m@�\)@���@��@�@��@�@�`B@��@��u@�9X@��@���@�S�@��@�n�@�@��@���@�j@���@���@�;d@��y@��+@��@�@��h@�G�@���@��@���@�Z@�b@��m@��@�S�@�C�@��@��\@��T@�p�@��`@��D@�1'@� �@���@�33@��H@��!@�ff@�E�@�$�@��@��@�p�@��@�Q�@�9X@�b@��
@�K�@���@��H@���@��#@��@�O�@�/@��@���@�I�@���@�33@���@�J@��@���@�l�@�bN@�r�@~��@t(�@j��@c"�@Y��@Q�^@IX@B~�@:��@5p�@.��@)&�@$j@\)@�@`B@�\@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A���A��A���A���A���A���A��A��A���A���AޮAީ�Aޣ�Aޝ�Aޕ�AލPAމ7AރA�z�AݶFA���A���A�oA�^5A�bA�l�A̡�A��A�(�A�+A�A�A���A�p�A��A���A��mA�"�A��PA���A��A��hA��\A�ȴA��A�ĜA�E�A��;A��A�VA��A���A�oA���A�  A��;A���A�I�A���A�jA�5?A�p�A�
=A�=qA�bNA��-A�A�A�S�A���A�bNAy�Aq�Al�AfffAa"�A]�A[|�AZjAX�AS��ARr�AN�AJM�AF�HAC�AA;dA<�yA9O�A8JA7�wA7��A7/A69XA6bNA6�A6�A733A7�;A7��A7�#A7ƨA7��A6��A4�DA1l�A1G�A2ĜA4�A4�A3�A2�A2�A2�uA2bNA1�A.�A,VA+�hA*��A)�mA)�
A)x�A)\)A(A�A'oA&~�A&$�A%�A#��A"�A"ĜA"�`A"��A!�^A!XA!"�A ^5A �A�;A��A��Ax�AhsA33A��A��A1'A  A��A�A33A�HA�9AM�A�A�uA5?AI�AA�A33A��A�-A33A�!A(�A�wA��A\)A�A��A��A^5A(�A�TA�hAdZA?}AA�uAE�A$�A�A�#A�^A�hA�;A�FA�^A��Ap�A�A�7A��A��A
=A��AjA �AbA��A�mAp�AG�A7LAVA�HA��AI�AJA�#AAXA�jAffA�;AG�A
�yA
��A
v�A
-A
A	��A�HAbA��AdZAoA��A�uAQ�A-A�
AO�A�/A�jAQ�A=qA�A�FAdZA��A��Az�AE�A  A�A�A ��A $�@�^5@�/@�Ĝ@�z�@�Q�@��@��F@��H@���@�-@��@��@�b@��@�+@���@�7L@��/@��;@�l�@�\@�v�@�n�@�@�X@�%@�j@�"�@�\@�M�@��@�`B@��`@�Z@�@��y@�^5@��@���@��@�p�@�j@�l�@���@���@�I�@���@��
@�t�@���@�E�@�G�@�9X@ߥ�@��@ޗ�@�=q@���@���@�hs@܋D@���@�l�@�+@��@�^5@ّh@��@��@�(�@ם�@�dZ@��@֏\@�J@�G�@���@ԋD@���@�+@҇+@���@ѩ�@�`B@Ѓ@��m@�K�@Ώ\@���@̋D@��m@ʇ+@�@��T@�p�@�j@���@ǥ�@�S�@�o@��y@��@őh@��@ēu@�1'@��;@öF@�|�@�|�@�l�@�M�@��h@�hs@�V@���@� �@��F@���@�n�@�E�@�J@�7L@��D@� �@�|�@�+@���@���@���@��^@�`B@��@�I�@��
@��P@�;d@��@��R@�=q@��@�`B@�7L@��/@�I�@��@�33@��@��+@�-@��#@��^@���@���@���@��h@�?}@��@���@�(�@��@��m@�\)@���@��@�@��@�@�`B@��@��u@�9X@��@���@�S�@��@�n�@�@��@���@�j@���@���@�;d@��y@��+@��@�@��h@�G�@���@��@���@�Z@�b@��m@��@�S�@�C�@��@��\@��T@�p�@��`@��D@�1'@� �@���@�33@��H@��!@�ff@�E�@�$�@��@��@�p�@��@�Q�@�9X@�b@��
@�K�@���@��H@���@��#@��@�O�@�/@��@���@�I�@���@�33@���@�J@��G�O�@�l�@�bN@�r�@~��@t(�@j��@c"�@Y��@Q�^@IX@B~�@:��@5p�@.��@)&�@$j@\)@�@`B@�\@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�
B
�
B
�
B
�
B
�
B
�B
�#B
�)B
�/B
�TB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB.B>wB>wB>wBYB�B��B�-BÖB��B�BXB�PB��B�B�jB�B�B�qBǮB��B�-B��B�\B�Bt�BdZBN�B7LB$�B�BPB��B�TB��B�}B�B��B�bBw�BVB49BhB
�mB
�FB
�hB
gmB
5?B	��B	�3B	z�B	YB	1'B	%�B	'�B	)�B	+B	/B	Q�B	ZB	cTB	cTB	n�B	p�B	_;B	B�B	B�B	VB	_;B	bNB	k�B	� B	�hB	��B	B	��B	�B	��B	��B	��B
B
DB	��B	�B
  B
�B
F�B
G�B
G�B
R�B
[#B
YB
W
B
O�B
A�B
6FB
2-B
.B
,B
2-B
<jB
D�B
A�B
:^B
6FB
33B
2-B
2-B
49B
@�B
F�B
J�B
F�B
I�B
M�B
J�B
L�B
R�B
XB
XB
XB
\)B
^5B
aHB
aHB
cTB
e`B
hsB
iyB
l�B
l�B
jB
ffB
bNB
aHB
`BB
bNB
bNB
YB
T�B
O�B
M�B
I�B
H�B
H�B
H�B
H�B
I�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
O�B
Q�B
Q�B
R�B
T�B
]/B
bNB
gmB
jB
gmB
o�B
q�B
k�B
m�B
t�B
t�B
r�B
p�B
p�B
s�B
s�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
n�B
m�B
l�B
iyB
gmB
e`B
bNB
aHB
aHB
aHB
`BB
^5B
\)B
VB
Q�B
N�B
M�B
L�B
K�B
J�B
H�B
G�B
E�B
C�B
B�B
B�B
C�B
B�B
A�B
A�B
B�B
@�B
@�B
?}B
>wB
=qB
<jB
:^B
;dB
:^B
6FB
49B
49B
49B
49B
33B
33B
33B
7LB
5?B
33B
2-B
1'B
1'B
1'B
1'B
/B
-B
,B
-B
.B
.B
-B
.B
/B
/B
-B
-B
-B
-B
,B
+B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
&�B
%�B
%�B
#�B
"�B
!�B
 �B
 �B
"�B
#�B
"�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
hB
\B
\B
VB
VB
JB
JB
JB
JB
JB
PB
JB
PB
PB
PB
JB
PB
PB
PB
VB
\B
\B
PB
JB
JB
DB
DB

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
JB
PB
PB
VB
VB
VB
VB
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
bB
bB
bB
bB
bB
bB
bB
bB
hB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
oB
oB
oB
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
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
#�B
+B
49B
9XB
@�B
F�B
J�B
O�B
S�B
YB
^5B
aHB
dZB
hsB
m�B
p�B
s�B
v�B
{�B
}�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�:B
�aB
�lB
�oB
�lB
�nB
�oB
�iB
�mB
�pB
�cB
�[B-�B>^B>^B>`BX�B�B��B�B�vBʤB�BW�B�3B�kB��B�OB��B��B�SBǒB�lB�B��B�CB� Bt�Bd<BN�B71B$�BiB3B��B�5B��B�`B��B��B�FBw�BU�B4BLB
�PB
�(B
�MB
gSB
5%B	��B	�B	z�B	YB	1B	%�B	'�B	)�B	*�B	/B	Q�B	ZB	c=B	c=B	n}B	p�B	_"B	ByB	BvB	U�B	_$B	b4B	kmB	�B	�LB	��B	�sB	��B	�B	��B	��B	��B
�B
%B	��B	�B	��B
�B
F�B
G�B
G�B
R�B
[B
X�B
V�B
O�B
AiB
6$B
2B
-�B
+�B
2B
<IB
D}B
AgB
:<B
6(B
3B
2B
2B
4B
@_B
F�B
J�B
F�B
I�B
M�B
J�B
L�B
R�B
W�B
W�B
W�B
\B
^B
a$B
a$B
c3B
e>B
hQB
iVB
ljB
lhB
j\B
fCB
b+B
a%B
`B
b,B
b-B
X�B
T�B
O�B
M�B
I�B
H�B
H�B
H�B
H�B
I�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
O�B
Q�B
Q�B
R�B
T�B
]B
b-B
gKB
jZB
gIB
o}B
q�B
kaB
moB
t�B
t�B
r�B
p�B
p�B
s�B
s�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
ozB
ntB
mmB
lgB
iTB
gKB
e>B
b*B
a#B
a%B
a#B
` B
^B
\B
U�B
Q�B
N�B
M�B
L�B
K�B
J�B
H�B
G�B
E�B
CsB
BnB
BnB
CqB
BoB
AfB
AeB
BnB
@`B
@`B
?YB
>TB
=MB
<IB
::B
;AB
:;B
6$B
4B
4B
4B
4B
3B
3B
3B
7*B
5B
3B
2B
1B
1B
1B
1B
.�B
,�B
+�B
,�B
-�B
-�B
,�B
-�B
.�B
.�B
,�B
,�B
,�B
,�B
+�B
*�B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
&�B
%�B
%�B
#�B
"�B
!�B
 �B
 �B
"�B
#�B
"�B
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
�B
�B
�B
{B
vB
rB
jB
fB
\B
fB
cB
eB
]B
VB
eB
dB
\B
^B
]B
`B
]B
\B
\B
VB
SB
DB
9B
7B
2B
3B
(B
'B
&B
'B
*B
+B
&B
,B
,B
,B
'B
+B
.B
,B
2B
9B
;B
,B
'B
'B
"B
 B

B

B

B

B
!B
!B
"B
'B
&B
)B
-B
,B
&B
-B
+B
3B
2B
0B
1B
0B
1B
8B
7B
:B
8B
7B
8B
9B
9B
>B
<B
?B
?B
>B
?B
=B
>B
EB
>B
@B
AB
?B
BB
?B
@B
?B
=B
@B
KB
KB
IB
LB
HB
JB
IB
KB
JB
SB
TB
PB
XB
XB
WB
WB
VB
SB
UB
\B
[B
]B
cB
bB
aB
bB
hB
hB
nB
pB
pB
vB
vB
sB
vB
xB
tB
yB
{B
yB
zB
tB
tB
uB
uB
yB
�B
�B
�B
�B
{B
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
�B
�B
�B
�B
�B
 �B
!�B
!�G�O�B
#�B
*�B
4B
92B
@\B
FB
J�B
O�B
S�B
X�B
^B
a"B
d3B
hIB
mjB
p|B
s�B
v�B
{�B
}�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544582016080715445820160807154458  AO  ARCAADJP                                                                    20150226221451    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221451  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221451  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154458  IP                  G�O�G�O�G�O�                