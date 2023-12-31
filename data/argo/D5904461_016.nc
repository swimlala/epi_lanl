CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:36Z AOML 3.0 creation; 2016-08-07T21:36:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221336  20160807143630  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_016                   2C  D   APEX                            6531                            072314                          846 @�&#v 1   @�&W:�@1yXbM��c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�ffB���B���B�  B�ffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD�� D�L�D���D��fD� D�FfD��fD��fD�3D�FfD�y�D�ɚD��D�33D�vfD�3D� D�FfD�3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@��
A�A#�AC�Ac�A�A�A�A�A�Aҏ\A�A�B �HB�HB�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B��
B�=qB�=qB�p�B��
B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C 8RC8RC8RC8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCLQ�CN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�GDy�zD��
D�S�D���D��pD�
D�MpD��pD��pD�
=D�MpD���D�ФD��D�:=D�}pD�=D�
D�MpD�=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A��/A��;A��HA��/A��#A��`A��mA��`A��A�A�A��A��
A���A�ȴA���AֶFAֶFAִ9Aְ!A֩�A֛�A�O�AՍPAԋDA�z�A��AˬA�VA��A��A�"�A�7LA�C�A��A�O�A�G�A���A�`BA��A��
A�%A�?}A�S�A�?}A�p�A� �A��\A�ȴA�ZA�S�A��A�O�A��A��DA�7LA���A�A�A��A���A���A�K�A���A�~�A�VA��A�(�A��PA��A�v�A�Q�A���A���A��A��uA��A��A�&�A�z�A�1A���A�/A�G�A��A��A�ZA�Az��AvZAt1AsG�AqhsAkAi��AcO�A^A[`BAZ�uAV��AVM�AU�hAR9XAN�jAL�+AI�hAH$�AFQ�AC��AAO�A@  A?+A>1'A<E�A9�A6��A6A�A5�^A5/A4��A4=qA3��A1�wA1"�A/�wA.-A,�+A, �A+p�A)hsA'�FA&�A&9XA%�^A%|�A%\)A$ȴA#��A"��A"A ��AA�A^5A��AE�AE�A��A��AC�A�A%A��AE�AG�A9XA��A��AZAG�A{A\)A-A�FAXAĜA�
AoA�A��AK�A
��A
VA	��A�HAffA&�A�AM�AXAȴA�F@�@�^5@��@��y@�~�@�?}@�F@�=q@��@��@��#@�hs@�O�@�O�@��@��`@�u@��@땁@��@���@�hs@�%@�  @�+@�R@�J@�@�p�@�@�ƨ@�C�@�5?@�Z@ݲ-@��
@�@���@��@���@��@�I�@�ƨ@�@�J@��`@� �@Ͼw@θR@���@ͩ�@̬@�  @�@�=q@��#@�hs@���@��@�\)@�o@���@�J@őh@�&�@�Z@�\)@��@�ȴ@��H@��@���@�ȴ@�~�@�n�@�V@��@�hs@���@���@�Ĝ@��j@��u@�I�@�9X@��;@��@���@�v�@��+@�-@���@��7@�X@��@���@�j@�b@��@���@���@�J@�@��T@�J@��^@�7L@�`B@�hs@�`B@�X@�G�@��@��/@�z�@��;@��F@��;@��m@�ƨ@��F@�l�@�+@��@��R@�ff@�=q@�@��@��j@�1'@�1'@���@���@��@�r�@���@��9@��@�  @�\)@��@��@��y@�~�@�5?@�@��#@��^@��@�1@���@���@�\)@��y@�ȴ@�ff@��#@���@��h@�/@���@�1'@��w@�dZ@���@�l�@��@��R@�v�@�J@��#@��@���@�V@���@���@���@�
=@��@�n�@��-@�G�@�&�@��/@��@��j@�9X@���@�l�@�K�@�;d@�
=@��!@��!@�5?@�-@�-@�$�@�-@���@�p�@�hs@��/@�\)@�M�@��^@�x�@�&�@�%@��9@�bN@���@�z�@��
@�dZ@��y@���@�v�@�=q@���@���@��h@��@�`B@�&�@���@��9@�9X@�1@���@�ƨ@���@��@�K�@�@��@���@�^5@�M�@�5?@�J@��T@���@���@�p�@�/@���@��j@��@�9X@�b@�1@���@���@�S�@�C�@��H@���@�{@��^@���@�p�@�O�@�?}@��@�V@�%@���@���@�z�@�A�@��m@��@�S�@�"�@���@���@�^5@�M�@�5?@�{@��@��-@��7@�hs@�G�@���@�(�@�  @�ƨ@�dZ@�;d@�"�@�o@��@���@�b@|�D@r��@j��@_��@W;d@M�T@E�T@@b@8�u@1��@,z�@&ȴ@!��@�D@�@dZ@+@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A��/A��;A��HA��/A��#A��`A��mA��`A��A�A�A��A��
A���A�ȴA���AֶFAֶFAִ9Aְ!A֩�A֛�A�O�AՍPAԋDA�z�A��AˬA�VA��A��A�"�A�7LA�C�A��A�O�A�G�A���A�`BA��A��
A�%A�?}A�S�A�?}A�p�A� �A��\A�ȴA�ZA�S�A��A�O�A��A��DA�7LA���A�A�A��A���A���A�K�A���A�~�A�VA��A�(�A��PA��A�v�A�Q�A���A���A��A��uA��A��A�&�A�z�A�1A���A�/A�G�A��A��A�ZA�Az��AvZAt1AsG�AqhsAkAi��AcO�A^A[`BAZ�uAV��AVM�AU�hAR9XAN�jAL�+AI�hAH$�AFQ�AC��AAO�A@  A?+A>1'A<E�A9�A6��A6A�A5�^A5/A4��A4=qA3��A1�wA1"�A/�wA.-A,�+A, �A+p�A)hsA'�FA&�A&9XA%�^A%|�A%\)A$ȴA#��A"��A"A ��AA�A^5A��AE�AE�A��A��AC�A�A%A��AE�AG�A9XA��A��AZAG�A{A\)A-A�FAXAĜA�
AoA�A��AK�A
��A
VA	��A�HAffA&�A�AM�AXAȴA�F@�@�^5@��@��y@�~�@�?}@�F@�=q@��@��@��#@�hs@�O�@�O�@��@��`@�u@��@땁@��@���@�hs@�%@�  @�+@�R@�J@�@�p�@�@�ƨ@�C�@�5?@�Z@ݲ-@��
@�@���@��@���@��@�I�@�ƨ@�@�J@��`@� �@Ͼw@θR@���@ͩ�@̬@�  @�@�=q@��#@�hs@���@��@�\)@�o@���@�J@őh@�&�@�Z@�\)@��@�ȴ@��H@��@���@�ȴ@�~�@�n�@�V@��@�hs@���@���@�Ĝ@��j@��u@�I�@�9X@��;@��@���@�v�@��+@�-@���@��7@�X@��@���@�j@�b@��@���@���@�J@�@��T@�J@��^@�7L@�`B@�hs@�`B@�X@�G�@��@��/@�z�@��;@��F@��;@��m@�ƨ@��F@�l�@�+@��@��R@�ff@�=q@�@��@��j@�1'@�1'@���@���@��@�r�@���@��9@��@�  @�\)@��@��@��y@�~�@�5?@�@��#@��^@��@�1@���@���@�\)@��y@�ȴ@�ff@��#@���@��h@�/@���@�1'@��w@�dZ@���@�l�@��@��R@�v�@�J@��#@��@���@�V@���@���@���@�
=@��@�n�@��-@�G�@�&�@��/@��@��j@�9X@���@�l�@�K�@�;d@�
=@��!@��!@�5?@�-@�-@�$�@�-@���@�p�@�hs@��/@�\)@�M�@��^@�x�@�&�@�%@��9@�bN@���@�z�@��
@�dZ@��y@���@�v�@�=q@���@���@��h@��@�`B@�&�@���@��9@�9X@�1@���@�ƨ@���@��@�K�@�@��@���@�^5@�M�@�5?@�J@��T@���@���@�p�@�/@���@��j@��@�9X@�b@�1@���@���@�S�@�C�@��H@���@�{@��^@���@�p�@�O�@�?}@��@�V@�%@���@���@�z�@�A�@��m@��@�S�@�"�@���@���@�^5@�M�@�5?@�{@��@��-@��7@�hs@�G�@���@�(�@�  @�ƨ@�dZ@�;d@�"�@�oG�O�@���@�b@|�D@r��@j��@_��@W;d@M�T@E�T@@b@8�u@1��@,z�@&ȴ@!��@�D@�@dZ@+@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
� B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
~�B
}�B
�B
�B
�B
�=B
��B
��B
�3B
��B
��B
��B
��B
�}B
��B
B
ƨB
ǮB
��B
�mB�B8RB[#B��B�B�JB��B�B��B��B�B$�B[#B[#BffBu�B|�B�DB��B��B�5B��B�wB��B��B�\B�%BffB]/BQ�BM�BL�BM�BL�BD�B9XB/B�B�B��B�BB�FB�VBE�BB
�#B
�-B
��B
��B
��B
�=B
�hB
��B
��B
��B
��B
��B
�B
x�B
jB
H�B
/B
bB	�ZB	ŢB	��B	��B	�hB	�B	YB	H�B	#�B	%B��B�B�ZB�BB�#B��BÖB��B�}B�XB�LB�?B�?B�XB�RB�FB�FB�?B�RB�RB�LB�LB�LB�FB�?B�-B�B�B�B�B�B�B�9B�LB�^B��BɺB��B��B�B�
B�B��B��BǮBŢBĜBŢBĜB��B��B��B��B��B��B��B��B��B�/B�B��B��B�B�B�B�B�B��B��B��B��B��B	B		7B	JB	PB	hB	{B	{B	JB	  B	B	�B	B��B�HB��B��B��B��B��B�
B�B�)B�ZB�yB�B�B�B�B�B�B��B��B��B	  B	  B	  B	B	B	1B	
=B	
=B	
=B	
=B	
=B	DB	
=B	
=B	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	&�B	'�B	.B	/B	/B	2-B	49B	7LB	:^B	;dB	=qB	?}B	B�B	F�B	G�B	H�B	J�B	L�B	M�B	O�B	Q�B	T�B	YB	]/B	^5B	^5B	^5B	cTB	dZB	e`B	iyB	n�B	s�B	u�B	u�B	u�B	v�B	x�B	x�B	|�B	~�B	� B	�B	�%B	�%B	�7B	�JB	�VB	�VB	�\B	�bB	�hB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�3B	�9B	�LB	�^B	�wB	�wB	��B	��B	ÖB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�#B	�B	�B	�)B	�/B	�/B	�/B	�;B	�BB	�ZB	�`B	�ZB	�fB	�mB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
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
%B
%B
%B
%B
%B
%B
+B
PB
�B
�B
!�B
&�B
,B
49B
;dB
B�B
H�B
N�B
S�B
ZB
^5B
bNB
gmB
l�B
o�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
��B
�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
~�B
}�B
�B
�B
��B
�1B
��B
��B
�%B
�tB
�vB
�vB
�tB
�pB
�|B
B
ƜB
ǡB
��B
�^BqB8BB[B��B��B�:B��B��B�uB˰B�}B$�B[B[BfVBu�B|�B�0B�tB��B�&B��B�hB��B��B�JB�BfTB]BQ�BM�BL�BM�BL�BD�B9DB/	B�BuB��B�0B�3B�CBE�B �B
�B
�B
��B
��B
�zB
�-B
�WB
��B
��B
��B
��B
��B
�B
x�B
jpB
H�B
/B
TB	�NB	ŗB	��B	��B	�]B	��B	YB	H�B	#�B	B��B�B�TB�=B�B��BÒB��B�yB�QB�FB�=B�:B�TB�MB�BB�DB�=B�NB�LB�HB�EB�GB�DB�<B�(B�B�B�B�
B�
B�B�5B�FB�XB�zBɵB��B��B�B�B��B��B��BǦBŚBĘBŚBĖBʺB˾B��B��B��B��B��B��B��B�&B�B��B��B�B�B�B�B�B��B��B��B��B��B	B		+B	?B	GB	]B	oB	oB	@B��B	B	vB	B��B�=B��B��B��B��B��B��B�B�B�RB�nB�B�B�B�B�B�B��B��B��B��B��B��B	B	B	&B	
2B	
2B	
2B	
0B	
.B	6B	
/B	
0B	WB	rB	rB	tB	�B	�B	�B	�B	�B	�B	 �B	$�B	&�B	'�B	.B	/B	/B	2B	4*B	7;B	:OB	;UB	=cB	?oB	BB	F�B	G�B	H�B	J�B	L�B	M�B	O�B	Q�B	T�B	YB	]B	^$B	^$B	^%B	cCB	dIB	eMB	iiB	n�B	s�B	u�B	u�B	u�B	v�B	x�B	x�B	|�B	~�B	�B	��B	�B	�B	�$B	�6B	�DB	�EB	�KB	�QB	�VB	�RB	�PB	�\B	�iB	�nB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�	B	�	B	�B	�B	�B	�B	�B	�B	�%B	�;B	�KB	�dB	�dB	�pB	�uB	ÀB	ƔB	ǚB	ǚB	ǜB	ǛB	ȞB	ɨB	ɧB	ɦB	ȠB	ɧB	̷B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�(B	�,B	�EB	�JB	�DB	�PB	�YB	�MB	�HB	�KB	�IB	�RB	�QB	�XB	�^B	�]B	�^B	�iB	�qB	�qB	�uB	�}B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�uB	�cB	�^B	�XB	�PB	�PB	�QB	�]B	�|B	�B	�|B	�|B	�tB	�uB	�sB	�tB	�wB	�uB	�vB	�zB	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
G�O�B
8B
qB
�B
!�B
&�B
+�B
4"B
;KB
ByB
H�B
N�B
S�B
ZB
^B
b6B
gTB
lqB
o�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.22 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436302016080714363020160807143630  AO  ARCAADJP                                                                    20150226221336    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221336  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221336  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143630  IP                  G�O�G�O�G�O�                