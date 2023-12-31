CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:32Z AOML 3.0 creation; 2016-06-01T00:08:14Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230832  20160531170814  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               4A   AO  4055_7112_052                   2C  D   APEX                            5374                            041511                          846 @֯��[ 1   @ְ Es@9�z�G��c�j~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    4A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�3D�VfD��3D�s3D� D�P D���D�� D��D�9�D�i�D�ٚD�3D�<�DږfD�ɚD���D�)�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@�RBHQ�BO�BW�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDtxRDy�D��D�X�D���D�u�D��D�R�D��\D��D�\D�<)D�l)D��)D��D�?\Dژ�D��)D��\D�,)D�l)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��;A�AϓuAϙ�AϏ\A�n�A�VA�G�A�G�A�&�A��A�{A�1A���A�ȴA�jA;wA�VA��A�7LA�\)A��yA�1AÅA��A���A�K�A�1'A� �A���A�v�A��PA� �A���A� �A�S�A�ffA���A���A�A�ĜA�A��A���A�9XA��uA�A�A��9A�dZA�(�A�XA��`A�hsA��A�1'A�A��A�(�A��A��!A�r�A�=qA�+A���A�A���A�VA��A���A���A�A��^A��A�A�I�A���A�`BA�%A�~�A��#A�bA��\A�;dA�?}A���A��-A��`A��A��A�
=A��A��A��A���A��+A���A�hsA�M�A��A��A��Ay�7Ax^5At��Arr�Aot�Aj��Ad^5Ab1A`�A_�mA_t�A_VA^JA]7LAY��AW;dAV$�AUXAT��AT(�AS\)ARZAQ��AP{AM�wAKp�AJ�AI�mAI/AI
=AH�+AF��AD�ADbAB�`AAƨA@$�A?A>�A=A=�A<bNA;��A;�7A;oA9��A9\)A8�A8Q�A7p�A6��A6$�A5��A5�PA5S�A533A5"�A4�A4jA3"�A2E�A0�A/��A/dZA.�+A-�7A,M�A*��A*1A)��A)|�A(�A'G�A&^5A%��A$�A${A#/A"-A ��A �9A z�A (�A�A"�AI�A�A�\Ar�AVA�TA�A
=A=qA`BA%Az�A�wA�AȴA�\AXA��A\)A�A �A�/A��AE�A�AƨA/A
�/A
�DA
M�A
JA	��A�\A��A��AM�A"�Av�A�A&�A ZA -@�K�@�|�@�@�p�@���@�Q�@�o@��9@�@�R@�j@�@���@���@���@�"�@��@�w@��@��@�  @�
=@�{@ܓu@�K�@�~�@٩�@�j@�
=@�@��@�+@��@���@�S�@�dZ@͙�@�C�@��#@�G�@�Z@ǶF@Ǖ�@�dZ@�
=@�@�/@�j@�l�@�@�X@��@�z�@�I�@���@�+@�v�@��7@���@�1@�C�@���@�J@���@���@�K�@���@��\@�@�(�@�~�@��@�G�@��@��@�l�@�ȴ@�=q@�@��@��@���@�(�@�\)@��R@��@�`B@��@��@�1'@��
@�S�@���@�n�@�-@���@���@��7@�V@��D@�Z@��@�|�@�K�@��R@���@�O�@���@�z�@�1@�;d@�ȴ@��+@�{@��@��@�9X@�l�@��y@��@�~�@�5?@��#@�x�@�%@���@���@�(�@���@�S�@��y@���@���@��^@�x�@�/@��@� �@��
@���@�"�@���@�-@���@�hs@��@�j@� �@��@��H@��!@���@�E�@�J@��@���@��h@�?}@���@�A�@��m@�t�@�;d@�o@�o@�@��y@��!@�v�@�M�@��@��T@��@��@��9@�Q�@���@��F@���@���@���@��P@�t�@�S�@�o@��R@���@���@��\@��+@�v�@��@��^@���@�x�@�&�@��`@�Ĝ@���@�j@��@��;@���@��w@���@�l�@�o@�ȴ@�v�@�M�@�E�@��@��@��^@���@��h@��@�G�@�&�@���@�z�@�Z@�I�@� �@�1@��@l�@~��@~ff@}�T@}p�@}/@|��@|z�@|�@{�m@{�F@{��@{t�@{33@z��@z�\@zn�@z^5@z-@z�@y�@x��@x1'@x �@x  @x  @w�@w�;@w��@w+@v��@u�@u�@r�@g
=@`Ĝ@V�y@Q�7@Ix�@D��@>��@6�@0��@+33@'�@"n�@�/@A�@��@��@�@
��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��;A�AϓuAϙ�AϏ\A�n�A�VA�G�A�G�A�&�A��A�{A�1A���A�ȴA�jA;wA�VA��A�7LA�\)A��yA�1AÅA��A���A�K�A�1'A� �A���A�v�A��PA� �A���A� �A�S�A�ffA���A���A�A�ĜA�A��A���A�9XA��uA�A�A��9A�dZA�(�A�XA��`A�hsA��A�1'A�A��A�(�A��A��!A�r�A�=qA�+A���A�A���A�VA��A���A���A�A��^A��A�A�I�A���A�`BA�%A�~�A��#A�bA��\A�;dA�?}A���A��-A��`A��A��A�
=A��A��A��A���A��+A���A�hsA�M�A��A��A��Ay�7Ax^5At��Arr�Aot�Aj��Ad^5Ab1A`�A_�mA_t�A_VA^JA]7LAY��AW;dAV$�AUXAT��AT(�AS\)ARZAQ��AP{AM�wAKp�AJ�AI�mAI/AI
=AH�+AF��AD�ADbAB�`AAƨA@$�A?A>�A=A=�A<bNA;��A;�7A;oA9��A9\)A8�A8Q�A7p�A6��A6$�A5��A5�PA5S�A533A5"�A4�A4jA3"�A2E�A0�A/��A/dZA.�+A-�7A,M�A*��A*1A)��A)|�A(�A'G�A&^5A%��A$�A${A#/A"-A ��A �9A z�A (�A�A"�AI�A�A�\Ar�AVA�TA�A
=A=qA`BA%Az�A�wA�AȴA�\AXA��A\)A�A �A�/A��AE�A�AƨA/A
�/A
�DA
M�A
JA	��A�\A��A��AM�A"�Av�A�A&�A ZA -@�K�@�|�@�@�p�@���@�Q�@�o@��9@�@�R@�j@�@���@���@���@�"�@��@�w@��@��@�  @�
=@�{@ܓu@�K�@�~�@٩�@�j@�
=@�@��@�+@��@���@�S�@�dZ@͙�@�C�@��#@�G�@�Z@ǶF@Ǖ�@�dZ@�
=@�@�/@�j@�l�@�@�X@��@�z�@�I�@���@�+@�v�@��7@���@�1@�C�@���@�J@���@���@�K�@���@��\@�@�(�@�~�@��@�G�@��@��@�l�@�ȴ@�=q@�@��@��@���@�(�@�\)@��R@��@�`B@��@��@�1'@��
@�S�@���@�n�@�-@���@���@��7@�V@��D@�Z@��@�|�@�K�@��R@���@�O�@���@�z�@�1@�;d@�ȴ@��+@�{@��@��@�9X@�l�@��y@��@�~�@�5?@��#@�x�@�%@���@���@�(�@���@�S�@��y@���@���@��^@�x�@�/@��@� �@��
@���@�"�@���@�-@���@�hs@��@�j@� �@��@��H@��!@���@�E�@�J@��@���@��h@�?}@���@�A�@��m@�t�@�;d@�o@�o@�@��y@��!@�v�@�M�@��@��T@��@��@��9@�Q�@���@��F@���@���@���@��P@�t�@�S�@�o@��R@���@���@��\@��+@�v�@��@��^@���@�x�@�&�@��`@�Ĝ@���@�j@��@��;@���@��w@���@�l�@�o@�ȴ@�v�@�M�@�E�@��@��@��^@���@��h@��@�G�@�&�@���@�z�@�Z@�I�@� �@�1@��@l�@~��@~ff@}�T@}p�@}/@|��@|z�@|�@{�m@{�F@{��@{t�@{33@z��@z�\@zn�@z^5@z-@z�@y�@x��@x1'@x �@x  @x  @w�@w�;@w��@w+@v��@u�@u�@r�@g
=@`Ĝ@V�y@Q�7@Ix�@D��@>��@6�@0��@+33@'�@"n�@�/@A�@��@��@�@
��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBQ�BP�BP�BQ�BQ�BQ�BP�BO�BN�BN�BL�BL�BM�BM�BL�BK�BI�BB�B;dB6FB)�B�B%B�yB�`B��B�FB��B��B��B��B��B��B��B��B�oB�\B�=B�B{�Bl�Be`BbNB]/BR�BQ�BS�BdZBbNB]/BXBJ�BA�B5?B)�B�B�B	7BB��B��B��B��B��B��B��B��B�B�B�B�B�ZB�HB�/B��BĜB�XB�B��B��B�Bq�BcTBW
B6FB��B��B�\Bs�B[#BI�B5?B\B
�sB
��B
�dB
�B
��B
�hB
z�B
]/B
,B	��B	�B	��B	��B	��B	�B	\)B	L�B	D�B	>wB	;dB	7LB	0!B	'�B	{B	1B	B��B��B��B�B�B�sB�BB�B��B��B��B��B��B��BɺBŢB��B�}B��B��B�wB�jB�^B�LB�3B�!B�'B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�JB�+B�B�B�B{�Bv�Bt�Br�Bp�Bo�Bm�BjBiyBhsBgmBffBe`BcTBbNB`BB^5B]/B\)B[#BXBT�BR�BP�BN�BM�BL�BK�BJ�BG�BC�B@�B>wB<jB9XB7LB6FB49B1'B/B.B.B-B,B)�B'�B$�B"�B�B�B�B�B�B�B�B�B{BoBoBhBbB\BPBJBDB
=B1B+B%B%B+B+B+B+B1B1B+B+B+B	7B
=B
=B
=B
=B
=BDBPBPB
=B%B%BDBJB%BBBB%B	7BDBPBhB{B�B�B�B�B�B �B�B!�B!�B"�B#�B%�B&�B'�B(�B+B)�B,B/B0!B0!B0!B49B9XB9XB;dB=qB=qB?}B@�BB�BC�BC�BD�BE�BG�BI�BJ�BL�BN�BQ�BR�BS�BT�BW
BYBZB[#B]/B]/B]/B_;BbNBcTBcTBffBgmBiyBl�Bp�Br�Bs�Bu�By�B|�B}�B� B�B�+B�7B�VB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�9B�LB�^B�jB�qB��BÖBƨBȴB��B��B��B��B�B�/B�;B�BB�NB�ZB�fB�mB�B�B��B��B��B��B	  B	B	B	B	%B	1B	DB	PB	\B	bB	uB	�B	�B	�B	�B	!�B	#�B	#�B	#�B	$�B	%�B	&�B	(�B	,B	-B	.B	.B	/B	/B	49B	7LB	7LB	9XB	<jB	?}B	@�B	A�B	C�B	F�B	I�B	J�B	K�B	L�B	O�B	S�B	XB	[#B	]/B	]/B	^5B	`BB	bNB	cTB	cTB	dZB	ffB	gmB	jB	l�B	n�B	n�B	p�B	q�B	s�B	t�B	w�B	x�B	z�B	|�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�JB	�JB	�JB	�PB	�PB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�B	�fB	��B
+B
{B
�B
$�B
2-B
;dB
D�B
K�B
P�B
YB
_;B
dZB
iyB
l�B
p�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BQ�BP�BP�BQ�BQ�BQ�BP�BO�BN�BN�BL�BL�BM�BM�BL�BK�BI�BB�B;gB6MB*B�B'B�yB�cB��B�DB��B��B��B��B��B��B��B��B�nB�[B�>B�B{�Bl�Be[BbKB].BR�BQ�BS�BdXBbIB]-BXBJ�BA�B59B)�B�B~B	1B	B��B��B��B��B��B��B��B��B�B�B�B�B�TB�EB�(B��BĘB�SB�B��B��B�Bq�BcKBWB6>B��B��B�TBs�B[BI�B59BWB
�nB
��B
�`B
�B
��B
�eB
z�B
]*B
,B	��B	�B	��B	��B	��B	�B	\.B	L�B	D�B	>{B	;jB	7RB	0*B	'�B	�B	;B	B��B��B��B�B�B�|B�MB�!B��B��B��B��B��B��B��BūB��B��B��B��B��B�vB�iB�WB�AB�+B�2B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�WB�9B�'B� B�B{�Bv�Bt�Br�Bp�Bo�Bm�Bj�Bi�Bh�Bg|BfuBeoBcbBb]B`QB^CB];B\7B[1BXBUBR�BP�BN�BM�BL�BK�BJ�BG�BC�B@�B>�B<|B9hB7]B6VB4LB18B/,B.&B.$B-B,B*B'�B$�B"�B�B�B�B�B�B�B�B�B�BeBdB^B[BlBbB?BVB
1B&B BBB B:B!B BCB&B:B;B<B	+B
4B
4B
5B
1B
NBQB`BCB
2B6BB:BZB6B(BBB3B	+B8BFB\B�B�B�B�B�B�B �B�B!�B!�B"�B#�B%�B&�B(B)B+B*	B,B/*B00B0-B01B4GB9eB9fB;qB=B=�B?�B@�BB�BC�BC�BD�BE�BG�BI�BJ�BL�BN�BQ�BS BTBU	BWBY$BZ*B[.B];B]9B]7B_FBbWBc]Bc_BfpBgzBi�Bl�Bp�Br�Bs�Bu�By�B|�B}�B�B�B�5B�BB�`B�{B�yB��B��B��B��B��B��B��B��B��B��B�B�B�*B�.B�;B�?B�SB�eB�pB�vB��BÞBƬBȻB��B��B��B��B�B�2B�>B�FB�SB�`B�jB�tB�B�B��B��B��B��B	 B	B	B	 B	*B	6B	GB	SB	bB	dB	yB	�B	�B	�B	�B	!�B	#�B	#�B	#�B	$�B	%�B	&�B	(�B	,B	-B	.B	.B	/B	/B	48B	7NB	7QB	9ZB	<kB	?�B	@�B	A�B	C�B	F�B	I�B	J�B	K�B	L�B	O�B	S�B	XB	[%B	]/B	]-B	^5B	`BB	bOB	cVB	cWB	d]B	fdB	goB	jB	l�B	n�B	n�B	p�B	q�B	s�B	t�B	w�B	x�B	z�B	|�B	}�B	�B	�B	�B	�B	�#B	�+B	�/B	�6B	�BB	�FB	�IB	�GB	�QB	�QB	�VB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	� B	�cB	��B
'B
wB
�B
$�B
2(B
;_B
D�B
K�B
P�B
YB
_5B
dTB
itB
l�B
p�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708142016053117081420160531170814  AO  ARCAADJP                                                                    20140721230832    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230832  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230832  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170814  IP                  G�O�G�O�G�O�                