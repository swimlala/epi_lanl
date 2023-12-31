CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:46Z AOML 3.0 creation; 2016-06-01T00:08:18Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230846  20160531170818  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               PA   AO  4055_7112_080                   2C  D   APEX                            5374                            041511                          846 @���+l��1   @����� @:�5?|��d�-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    PA   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyy�D�	�D�6fD�� D�� D��D�6fD�l�D���D�fD�C3D�i�Dǳ3D�	�D�,�Dڙ�D��fD�	�D�FfD�|�D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt{�Dy��D�)D�@�D���D���D�\D�@�D�w\D��\D��D�M�D�t)Dǽ�D�)D�7\Dڤ)D���D�)D�P�D�\D�z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��^A�ĜA���A���A�;dA�=qA�;dA�33A���A�XA�ĜA���A�;dA���A�;dA��hA���A���A�ffA�z�A���A���A���A���A��;A���A��`A�%A�\)A���A���A�bNA��A�v�A���A�jA��-A�(�A���A��DA�n�A�XA�?}A��A��FA�C�A���A��RA���A��A�G�A�
=A���A�S�A�+A�jA�A���A�XA���A�bNA�A�^5A��A��A���A�p�A�S�A�?}A�VA�VA�5?A���A���A�/A���A���A�"�A�A�A���A�(�A�oA�A���A��A��uA��A���A�K�A���A� �A��A��jA��A�`BA���A��/A���A��A��;A��A��A|1'Aw��At��Ar��Aqt�Aq%ApȴAp�Ao�FAnffAl��Aj��Ag�Ae��Ab�jA`bA_33A^�DA\�A\�A[;dAZ��AY�-AYK�AX�AX�uAW��AVVAU�PAU`BAT�yAS�AQ�^AN5?AL�AL5?AK�AIt�AIC�AFbNAB��AA�7AA"�A@��A@ȴA@^5A?ƨA?G�A>�yA>�\A>bNA=�TA=��A<��A:��A8�A7%A5��A5VA4jA3��A3A3�hA3dZA3/A2�jA2{A1G�A0^5A/�
A/XA.ĜA-�A-;dA,�\A+�A)��A)K�A(n�A'�FA'33A&~�A%�FA%�A"��A jA�A��A��AZA�A�AA�FA��A��A�A��Az�A^5A��A��A`BA+AoA��A��A7LAVA9XA��A��A  A?}A�/A7LA�jA��A�uA~�A��AS�A
��A	K�A-At�A�/A��A  A�/A�;AK�A=qA`BA �9@�n�@�ƨ@�z�@�b@��@�;d@�@���@���@���@�`B@�|�@�@���@�+@�@���@�F@�ȴ@�-@�V@���@���@�1@߶F@ߕ�@�33@ް!@�`B@�1'@��m@ۮ@�\)@ڰ!@ٺ^@�%@�dZ@�bN@�
=@���@��T@�dZ@ͺ^@̋D@�dZ@�5?@���@�bN@�+@�V@��T@���@��@�@�G�@�r�@�l�@�=q@��^@���@�x�@�`B@�7L@���@��9@���@�ƨ@��P@�t�@�
=@�
=@�o@���@��\@�?}@�r�@���@�|�@�o@�O�@���@�"�@�x�@���@��9@�bN@�  @���@���@�S�@���@�{@�G�@��`@�  @��m@���@��H@���@�Q�@���@�t�@���@��@��@���@�n�@�{@��h@��@�(�@��w@���@�|�@�S�@�C�@�33@��@��\@��@�hs@�&�@��D@�bN@�I�@���@�S�@�"�@�M�@�`B@���@�j@�1'@���@�t�@���@��@��H@�ȴ@��\@�=q@��7@��`@���@��9@���@��D@�r�@�Z@� �@�  @��
@���@�|�@�S�@�33@��@�@��!@�v�@��-@��^@���@� �@��9@�7L@�/@��F@��y@�M�@��#@��h@�hs@�r�@�  @��F@��@��@�o@��@�`B@�/@��@�V@�%@�%@���@��`@���@��j@��@���@��u@�r�@�A�@�b@��;@�ƨ@��@���@���@�\)@�33@���@�5?@���@��h@�V@��/@��j@�Q�@�\)@�"�@��@�o@�o@�o@�
=@�@���@��y@�ȴ@�v�@�@�@���@�x�@�?}@��@�j@�(�@�1@�w@|�@K�@
=@~�y@~ȴ@~�+@}�T@}p�@|��@|�j@|��@{�m@{ƨ@{ƨ@{ƨ@{��@{dZ@{o@u`B@m��@e`B@_�@W�@Q%@I��@D1@>��@8��@3�F@)7L@&��@!�@`B@�`@��@A�@�F@+@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��^A�ĜA���A���A�;dA�=qA�;dA�33A���A�XA�ĜA���A�;dA���A�;dA��hA���A���A�ffA�z�A���A���A���A���A��;A���A��`A�%A�\)A���A���A�bNA��A�v�A���A�jA��-A�(�A���A��DA�n�A�XA�?}A��A��FA�C�A���A��RA���A��A�G�A�
=A���A�S�A�+A�jA�A���A�XA���A�bNA�A�^5A��A��A���A�p�A�S�A�?}A�VA�VA�5?A���A���A�/A���A���A�"�A�A�A���A�(�A�oA�A���A��A��uA��A���A�K�A���A� �A��A��jA��A�`BA���A��/A���A��A��;A��A��A|1'Aw��At��Ar��Aqt�Aq%ApȴAp�Ao�FAnffAl��Aj��Ag�Ae��Ab�jA`bA_33A^�DA\�A\�A[;dAZ��AY�-AYK�AX�AX�uAW��AVVAU�PAU`BAT�yAS�AQ�^AN5?AL�AL5?AK�AIt�AIC�AFbNAB��AA�7AA"�A@��A@ȴA@^5A?ƨA?G�A>�yA>�\A>bNA=�TA=��A<��A:��A8�A7%A5��A5VA4jA3��A3A3�hA3dZA3/A2�jA2{A1G�A0^5A/�
A/XA.ĜA-�A-;dA,�\A+�A)��A)K�A(n�A'�FA'33A&~�A%�FA%�A"��A jA�A��A��AZA�A�AA�FA��A��A�A��Az�A^5A��A��A`BA+AoA��A��A7LAVA9XA��A��A  A?}A�/A7LA�jA��A�uA~�A��AS�A
��A	K�A-At�A�/A��A  A�/A�;AK�A=qA`BA �9@�n�@�ƨ@�z�@�b@��@�;d@�@���@���@���@�`B@�|�@�@���@�+@�@���@�F@�ȴ@�-@�V@���@���@�1@߶F@ߕ�@�33@ް!@�`B@�1'@��m@ۮ@�\)@ڰ!@ٺ^@�%@�dZ@�bN@�
=@���@��T@�dZ@ͺ^@̋D@�dZ@�5?@���@�bN@�+@�V@��T@���@��@�@�G�@�r�@�l�@�=q@��^@���@�x�@�`B@�7L@���@��9@���@�ƨ@��P@�t�@�
=@�
=@�o@���@��\@�?}@�r�@���@�|�@�o@�O�@���@�"�@�x�@���@��9@�bN@�  @���@���@�S�@���@�{@�G�@��`@�  @��m@���@��H@���@�Q�@���@�t�@���@��@��@���@�n�@�{@��h@��@�(�@��w@���@�|�@�S�@�C�@�33@��@��\@��@�hs@�&�@��D@�bN@�I�@���@�S�@�"�@�M�@�`B@���@�j@�1'@���@�t�@���@��@��H@�ȴ@��\@�=q@��7@��`@���@��9@���@��D@�r�@�Z@� �@�  @��
@���@�|�@�S�@�33@��@�@��!@�v�@��-@��^@���@� �@��9@�7L@�/@��F@��y@�M�@��#@��h@�hs@�r�@�  @��F@��@��@�o@��@�`B@�/@��@�V@�%@�%@���@��`@���@��j@��@���@��u@�r�@�A�@�b@��;@�ƨ@��@���@���@�\)@�33@���@�5?@���@��h@�V@��/@��j@�Q�@�\)@�"�@��@�o@�o@�o@�
=@�@���@��y@�ȴ@�v�@�@�@���@�x�@�?}@��@�j@�(�@�1@�w@|�@K�@
=@~�y@~ȴ@~�+@}�T@}p�@|��@|�j@|��@{�m@{ƨ@{ƨ@{ƨ@{��@{dZ@{o@u`B@m��@e`B@_�@W�@Q%@I��@D1@>��@8��@3�F@)7L@&��@!�@`B@�`@��@A�@�F@+@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�Bx�B�B��B��B
=BJBJBbBbB\BbB�B�B�B�B�B
=B��B�B�B�NB��B��BR�B,B�B��BaHBŢBPBjBt�B�B�PB��B�^BÖBƨBB�3B��B��B�B��B�yB��B��B��B��B�B�B�5B��B�?B��B��B�oB�DB�Bt�Bm�B`BBQ�BC�B/B'�B%�B"�B�BPB��B�B%B��B�B��B�wB�B��B��B��B�uB~�Bq�BcTBO�B.B �BB
�B
�;B
�)B
�
B
��B
�RB
�?B
�B
�=B
iyB
[#B
H�B
'�B
B	�B	�5B	�B	�B	�B	��B	��B	ƨB	�XB	�B	��B	�{B	~�B	p�B	jB	cTB	XB	P�B	J�B	F�B	B�B	A�B	@�B	C�B	F�B	E�B	C�B	A�B	=qB	5?B	)�B	�B	uB	bB	JB	1B	B��B��B�B�B�B�B�B�B�B�yB�sB�mB�`B�TB�;B�B��B��BȴBƨBĜBBB��B��B�}B�qB�^B�LB�?B�3B�'B�B�B��B��B��B��B��B��B��B�{B�oB�\B�DB�B� B|�By�Bv�Bt�Bs�Bs�Bs�Br�Br�Br�Bq�Bo�Bn�Bm�Bl�Bl�Bk�BjBjBiyBffB`BB\)BZBW
BT�BQ�BM�BH�BF�BE�BE�BE�BD�BB�B?}B;dB9XB6FB5?B33B2-B0!B-B+B(�B&�B$�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB�B�B�B�B!�B&�B)�B+B)�B(�B'�B'�B'�B'�B(�B+B,B.B0!B33B7LB:^B:^B;dB<jB=qB>wB>wB@�BC�BE�BD�BF�BG�BG�BG�BH�BL�BM�BM�BM�BL�BO�BS�BVB^5B`BB`BBaHBbNBcTBcTBe`BgmBgmBiyBiyBo�Bo�Bp�Bs�By�Bz�B{�B{�B� B�B�B�DB�VB�\B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�9B�?B�RB�wBÖBĜBĜBŢBƨBǮB��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�)B�BB�TB�yB�B�B�B�B��B��B	B	  B��B	  B	B	B	%B	+B	PB	\B	hB	uB	�B	�B	�B	#�B	%�B	&�B	&�B	'�B	'�B	'�B	(�B	(�B	)�B	+B	+B	,B	-B	.B	0!B	2-B	33B	49B	49B	49B	6FB	8RB	9XB	?}B	B�B	D�B	H�B	J�B	K�B	N�B	W
B	YB	ZB	ZB	ZB	ZB	[#B	[#B	[#B	[#B	\)B	_;B	cTB	e`B	ffB	gmB	hsB	n�B	p�B	r�B	s�B	t�B	u�B	u�B	w�B	w�B	w�B	x�B	z�B	}�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�JB	�oB	�B	ŢB	�BB	�B
B
VB
�B
 �B
-B
5?B
B�B
F�B
O�B
T�B
\)B
aHB
ffB
l�B
q�B
u�44444444444444444444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B�dB�B��B�#B��B��B�TB�$B��Bt�BmrB`&BQ�BCvB.�B'�B%�B"�B�B-B��B�BB��B��BʢB�VB��B��B��B�wB�WB~�Bq�Bc5BO�B-�B �B�B
�B
�B
�B
��B
�kB
�3B
� B
��B
� B
i\B
[B
H�B
'�B
�B	�sB	�B	��B	��B	��B	��B	��B	ƐB	�AB	��B	��B	�cB	~�B	p�B	jkB	c?B	W�B	P�B	J�B	F�B	BzB	AtB	@pB	C�B	F�B	E�B	C�B	AvB	=\B	5+B	)�B	�B	aB	PB	6B	B	B��B��B�B�B��B�B�B�xB�rB�gB�bB�[B�LB�AB�(B��B��B˳BȣBƗBĊB�|B�~B�wB�qB�kB�^B�MB�;B�.B�!B�B�	B��B��B��B��B��B��B��B�vB�jB�_B�NB�4B�B�B|�By�Bv�Bt�Bs�Bs�Bs�Br�Br�Br�Bq�Bo�Bn�Bm�BlyBl{BkuBjpBjqBihBfXB`6B\BZBV�BT�BQ�BM�BH�BF�BE�BE�BE�BD�BB�B?pB;UB9KB68B51B3&B2 B/�B- B*�B(�B&�B$�B!�B�B�B�B�B�B�B�B�B�B�BB�B}B�BvBrB�B�BB}BrBgBUB^B^B^BsBWBrB\B\B]B[BpBrBOBKB]BoB�B{B!�B&�B)�B*�B)�B(�B'�B'�B'�B'�B(�B*�B+�B-�B0B3"B7;B:LB:LB;SB<XB=`B>gB>dB@qBC�BE�BD�BF�BG�BG�BG�BH�BL�BM�BM�BM�BL�BO�BS�BU�B^!B`-B`.Ba4Bb8Bc>Bc?BeNBgYBgYBicBicBo�Bo�Bp�Bs�By�Bz�B{�B{�B�B��B�B�-B�?B�HB�YB�\B�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B� B�'B�:B�\B�{BăBăBŇBƌBǕBˬB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�;B�\B�oB�dB�sB�B��B��B	 �B��B��B��B	�B	�B	B	B	2B	?B	KB	YB	jB	dB	�B	#�B	%�B	&�B	&�B	'�B	'�B	'�B	(�B	(�B	)�B	*�B	*�B	+�B	,�B	-�B	0B	2B	3B	4B	4B	4B	6(B	85B	98B	?^B	BnB	D|B	H�B	J�B	K�B	N�B	V�B	X�B	Y�B	Y�B	Y�B	Y�B	[B	[B	[B	[B	\
B	_B	c3B	e>B	fDB	gMB	hSB	nwB	p�B	r�B	s�B	t�B	u�B	u�B	w�B	w�B	w�B	x�B	z�B	}�B	��B	��B	��B	��B	�B	�	B	�	B	�	B	�B	�(B	�JB	��B	�B	�B	�B
�B
1B
�B
 �B
,�B
5B
BiB
F�B
O�B
T�B
\B
aB
f?B
lfB
q�B
u�44444444444444444444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708182016053117081820160531170818  AO  ARCAADJP                                                                    20140721230846    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230846  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230846  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170818  IP                  G�O�G�O�G�O�                