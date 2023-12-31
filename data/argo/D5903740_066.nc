CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:39Z AOML 3.0 creation; 2016-06-01T00:08:16Z UW 3.1 conversion     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230839  20160531170816  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               BA   AO  4055_7112_066                   2C  D   APEX                            5374                            041511                          846 @���{�@1   @���
=@ @;2� ě��d((�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    BA   A   A   @333@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�	�D�6fD�s3D���D� D�L�D��fD�� D�fD�I�D�� D�� D� D�33DچfD���D�fD�33D�` D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @HQ�@��\@ʏ\AG�A#�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B0�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD��DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DtۅDy�D�)D�@�D�}�D��\D��D�W\D���D��D��D�T)D���D�ڏD��D�=�Dڐ�D��\D��D�=�D�j�D�m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�VA�VA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�XA�XA�ZA�XA�ZA�\)A�\)A�\)A�\)A�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�A��HA���A���A��HA��A�VA�?}A�ffA���A���A�p�A�%A�^5A�5?A��#A�7LA�9XA�?}A��+A�^5A���A�5?A�ƨA�K�A��A��yA��
A���A���A�-A�S�A���A�\)A�$�A��7A���A��DA�=qA�1A���A��^A��A���A� �A�^5A���A�{A�9XA���A�ffA��;A�v�A���A�E�A�oA�Q�A�O�A���A�=qA�ffA���A���A�?}A�ĜA���A�(�A��#A�x�A�A�1A�v�A�`BA���A��DA`BA| �AzbAx�jAwK�Au��AsC�An-AkƨAh-Ag��Af��Ad�A`�RA]|�A\A�AZĜAZA�AX�/AX$�AV�AU;dATjATJAS��AR��AQ/ALZAI�mAG�PAG�AG|�AG&�AF�AC��AC7LABbNAB^5ABI�ABQ�AB=qAB9XAB5?AB5?AB9XAB5?AB1'AB(�AB$�AB �AA��AAS�A@9XA>~�A=�PA<M�A9��A8��A8(�A7�hA6�yA6�DA5l�A3��A2��A1��A/XA-��A,1'A+VA*��A*1A)�A( �A&r�A%�^A$��A#�TA#A!�A!p�A!G�A!
=A ��A ~�A E�AC�AE�A��AhsA&�Ar�A7LAI�A��A|�A�\A�^A��A�mA��A`BAv�A��A
=A�\A��A�7AA��A$�A��A|�AG�A
�/A
�\A
1A��A(�At�A33A�HA��A1'A��AA{A��AĜA�A �H@�t�@��m@�V@�+@�33@���@�-@�@�-@���@�bN@��m@�$�@�&�@�A�@ް!@�(�@�=q@��`@�C�@���@�hs@���@җ�@љ�@�?}@�?}@��@��/@Ь@�z�@�1'@ϝ�@��@ͩ�@��@��/@�Z@� �@�1@�ƨ@�\)@��@Õ�@�~�@���@�33@���@�x�@��h@��#@���@� �@���@�7L@�Ĝ@�Q�@�  @���@��F@�t�@��!@�-@���@�X@��j@�r�@��@���@�dZ@�"�@��y@�ff@���@�O�@�V@��/@��@�j@�9X@�  @��
@���@�|�@��@��@��@��@��R@�@��@�p�@�7L@���@��j@��@�\)@��@���@�J@���@�G�@��@���@���@�1'@��;@�S�@��+@��@��#@�?}@��j@�(�@��;@��
@��P@�
=@��\@�v�@�v�@�v�@�V@�=q@�$�@��T@��h@�G�@���@��j@���@�r�@��@�ƨ@�t�@�o@���@�v�@�@���@�x�@��j@��@���@�l�@�;d@�o@��H@��+@�n�@�V@�E�@�=q@��^@��`@� �@�
=@���@�v�@��@�%@��@�Z@�I�@� �@�1@��m@��w@���@�K�@��H@��R@���@��+@�5?@�J@��-@��9@� �@��
@�|�@��y@�=q@��@��@��@��9@�r�@�  @�ƨ@��@�S�@�+@�"�@�o@�
=@���@��R@���@��+@�~�@�n�@�V@��@�p�@�&�@���@��/@��9@��@�1'@�w@~�y@~ff@~5?@~@}@}�h@}p�@}?}@|��@|�/@|��@|�j@|�j@|�@|�D@|Z@{�
@{t�@{S�@{C�@{33@{"�@{o@{@z��@y�^@y7L@y&�@x��@x  @w�@wK�@w;d@w
=@vff@vE�@v5?@v$�@s��@h�`@ahs@[�
@U?}@O�@K@D9X@>$�@4��@0�`@)G�@%p�@"=q@l�@A�@S�@�@��@��@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�VA�VA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�XA�XA�ZA�XA�ZA�\)A�\)A�\)A�\)A�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�A��HA���A���A��HA��A�VA�?}A�ffA���A���A�p�A�%A�^5A�5?A��#A�7LA�9XA�?}A��+A�^5A���A�5?A�ƨA�K�A��A��yA��
A���A���A�-A�S�A���A�\)A�$�A��7A���A��DA�=qA�1A���A��^A��A���A� �A�^5A���A�{A�9XA���A�ffA��;A�v�A���A�E�A�oA�Q�A�O�A���A�=qA�ffA���A���A�?}A�ĜA���A�(�A��#A�x�A�A�1A�v�A�`BA���A��DA`BA| �AzbAx�jAwK�Au��AsC�An-AkƨAh-Ag��Af��Ad�A`�RA]|�A\A�AZĜAZA�AX�/AX$�AV�AU;dATjATJAS��AR��AQ/ALZAI�mAG�PAG�AG|�AG&�AF�AC��AC7LABbNAB^5ABI�ABQ�AB=qAB9XAB5?AB5?AB9XAB5?AB1'AB(�AB$�AB �AA��AAS�A@9XA>~�A=�PA<M�A9��A8��A8(�A7�hA6�yA6�DA5l�A3��A2��A1��A/XA-��A,1'A+VA*��A*1A)�A( �A&r�A%�^A$��A#�TA#A!�A!p�A!G�A!
=A ��A ~�A E�AC�AE�A��AhsA&�Ar�A7LAI�A��A|�A�\A�^A��A�mA��A`BAv�A��A
=A�\A��A�7AA��A$�A��A|�AG�A
�/A
�\A
1A��A(�At�A33A�HA��A1'A��AA{A��AĜA�A �H@�t�@��m@�V@�+@�33@���@�-@�@�-@���@�bN@��m@�$�@�&�@�A�@ް!@�(�@�=q@��`@�C�@���@�hs@���@җ�@љ�@�?}@�?}@��@��/@Ь@�z�@�1'@ϝ�@��@ͩ�@��@��/@�Z@� �@�1@�ƨ@�\)@��@Õ�@�~�@���@�33@���@�x�@��h@��#@���@� �@���@�7L@�Ĝ@�Q�@�  @���@��F@�t�@��!@�-@���@�X@��j@�r�@��@���@�dZ@�"�@��y@�ff@���@�O�@�V@��/@��@�j@�9X@�  @��
@���@�|�@��@��@��@��@��R@�@��@�p�@�7L@���@��j@��@�\)@��@���@�J@���@�G�@��@���@���@�1'@��;@�S�@��+@��@��#@�?}@��j@�(�@��;@��
@��P@�
=@��\@�v�@�v�@�v�@�V@�=q@�$�@��T@��h@�G�@���@��j@���@�r�@��@�ƨ@�t�@�o@���@�v�@�@���@�x�@��j@��@���@�l�@�;d@�o@��H@��+@�n�@�V@�E�@�=q@��^@��`@� �@�
=@���@�v�@��@�%@��@�Z@�I�@� �@�1@��m@��w@���@�K�@��H@��R@���@��+@�5?@�J@��-@��9@� �@��
@�|�@��y@�=q@��@��@��@��9@�r�@�  @�ƨ@��@�S�@�+@�"�@�o@�
=@���@��R@���@��+@�~�@�n�@�V@��@�p�@�&�@���@��/@��9@��@�1'@�w@~�y@~ff@~5?@~@}@}�h@}p�@}?}@|��@|�/@|��@|�j@|�j@|�@|�D@|Z@{�
@{t�@{S�@{C�@{33@{"�@{o@{@z��@y�^@y7L@y&�@x��@x  @w�@wK�@w;d@w
=@vff@vE�@v5?@v$�@s��@h�`@ahs@[�
@U?}@O�@K@D9X@>$�@4��@0�`@)G�@%p�@"=q@l�@A�@S�@�@��@��@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B%�B&�B&�B%�B%�B%�B�B�B�B�B��BǮB�B��Bz�BjBI�B7LB.B�BB�B�BB��B��BÖB�?B�B��B��B��B��B��B��B�{B�7B�Bt�BiyBe`B`BBW
BN�BH�BB�B>wB9XB'�B�BbBB�TBɺB�dB�B��B�DB�Bx�Bm�BaHBM�B?}B/B$�B�BJB
��B
�B
�fB
��B
�'B
��B
��B
��B
�uB
�B
m�B
[#B
F�B
=qB
0!B
�B
JB
B	��B	�B	�;B	B	�'B	��B	��B	�VB	}�B	gmB	YB	P�B	I�B	G�B	D�B	@�B	8RB	1'B	,B	(�B	$�B	�B	oB	B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�TB�;B�)B��B�}B�XB�dB��B�qB��B�wB�XB�-B�B��B��B��B��B��B��B��B��B�\B�JB�=B�+B�B~�B{�B{�B{�B{�B}�B� B{�By�Bx�Bv�Bt�Bq�Bn�Bk�BiyBgmBdZBaHB^5B[#BXBT�BR�BP�BO�BN�BM�BK�BH�BE�BB�BA�B@�B?}B>wB<jB9XB8RB6FB5?B5?B49B33B2-B1'B/B-B+B)�B(�B&�B"�B�B�B{B\BJBJBJBPBPBJBDBDB
=B	7B1B1B	7B	7B	7B
=BDB
=BPBVB\B\B\B\B\B\B\B\B\BVBhB{B�B�B�B{BuB�B�B�B�B�B�B �B�B�B�B�B%�B&�B'�B(�B)�B)�B)�B)�B+B,B.B9XB;dB;dB=qB=qB>wB>wB?}B@�BB�BD�BD�BE�BE�BF�BG�BG�BH�BH�BI�BJ�BJ�BK�BP�BYB^5B_;B_;B`BBaHBaHBdZBgmBiyBiyBl�Bn�Bo�Bq�Br�Bt�Bu�Bv�Bx�B{�B� B�B�%B�=B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�LB�XB�^B�wBBŢBƨBǮBȴB��B��B��B��B��B��B��B�
B�)B�ZB�mB�sB�B�B��B��B��B��B��B��B��B	  B	B	%B	+B	+B	1B	
=B	DB	VB	�B	�B	�B	!�B	&�B	-B	5?B	9XB	;dB	=qB	?}B	D�B	E�B	H�B	J�B	L�B	M�B	M�B	N�B	N�B	P�B	R�B	S�B	S�B	T�B	VB	W
B	]/B	`BB	bNB	cTB	dZB	ffB	iyB	l�B	p�B	r�B	s�B	t�B	u�B	v�B	w�B	w�B	y�B	y�B	z�B	z�B	z�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�DB	�\B	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	ȴB	�HB	�B
B
B
�B
 �B
&�B
2-B
9XB
D�B
I�B
M�B
P�B
XB
^5B
cTB
ffB
l�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B%�B&�B&�B%�B%�B%�B�B�BBkB��BǙB��B��Bz�BjhBI�B73B-�BmBB�B�&B��BʣB�vB� B��B��B��B��B�|B�|B�uB�\B�B��Bt�BiYBeCB`$BV�BN�BH�BBrB>\B98B'�B�BEB�B�0BɝB�CB��B��B�&B��Bx�BmsBa%BM�B?^B.�B$�B�B-B
��B
�B
�GB
νB
�B
��B
��B
��B
�YB
��B
mvB
[B
F�B
=VB
0B
�B
0B
B	��B	�B	�!B	�vB	�B	��B	�sB	�AB	}�B	gWB	YB	P�B	I�B	G�B	D�B	@nB	8?B	1B	+�B	(�B	$�B	�B	\B	B��B��B��B��B��B��B�B�B�B�~B�~B�B�}B�|B�}B�B�}B�{B�}B�}B�~B�yB�sB�_B�AB�(B�B��B�kB�EB�QB�wB�aB�oB�eB�FB�B��B��B��B��B��B��B��B��B�uB�LB�<B�.B�B�B~�B{�B{�B{�B{�B}�B�B{�By�Bx�Bv�Bt�Bq�Bn�BktBijBg\BdKBa9B^&B[BXBT�BR�BP�BO�BN�BM�BK�BH�BE�BB�BA|B@uB?mB>gB<\B9JB8EB66B51B50B4+B3$B2B0�B/B-B*�B)�B(�B&�B"�B�BpBmBMB"B!B!B'B(B#BBB
B	(BBB	'B	B	B
B5B
B$B-B0B0BKBMBLBLBKBMBLB,B<BkBsBpBsBmBcB\BbB~B�B�B�B �B�B�B�B�B%�B&�B'�B(�B)�B)�B)�B)�B*�B+�B.B9GB;PB;QB=]B=_B>dB>fB?hB@oBB|BD�BD�BE�BE�BF�BG�BG�BH�BH�BI�BJ�BJ�BK�BP�BYB^ B_&B_'B`.Ba6Ba5BdDBgWBidBicBlvBn�Bo�Bq�Br�Bt�Bu�Bv�Bx�B{�B�B��B�B�'B�<B�QB�XB�iB�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�2B�<B�GB�[B�uBŉBƌBǔBȜBʧB̲BͷBͺB͹BͷB��B��B�B�>B�QB�VB�oB�B��B��B��B��B��B��B��B��B	�B	B	B	B	B	
 B	'B	:B	lB	�B	�B	!�B	&�B	,�B	5!B	96B	;GB	=PB	?`B	D}B	E�B	H�B	J�B	L�B	M�B	M�B	N�B	N�B	P�B	R�B	S�B	S�B	T�B	U�B	V�B	]B	` B	b,B	c3B	d:B	fDB	iXB	lmB	p�B	r�B	s�B	t�B	u�B	v�B	w�B	w�B	y�B	y�B	z�B	z�B	z�B	z�B	{�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�:B	�EB	�UB	�SB	�YB	�kB	�qB	�rB	�rB	��B	ȐB	�#B	�B
 �B
�B
[B
 �B
&�B
2B
92B
DvB
I�B
M�B
P�B
W�B
^B
c,B
f@B
leB
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708162016053117081620160531170816  AO  ARCAADJP                                                                    20140721230839    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230839  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230839  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170816  IP                  G�O�G�O�G�O�                