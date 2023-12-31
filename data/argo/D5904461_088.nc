CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-10T10:16:43Z AOML 3.0 creation; 2016-08-07T21:36:41Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151210101643  20160807143641  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               XA   AO  5286_8897_088                   2C  D   APEX                            6531                            072314                          846 @ׅ�J
1   @ׅ	���w@3a�7Kƨ�c+"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    XA   B   B   @�  @���@���A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dx��D�3D�C3D�s3D�� D��D�FfD��3D���D��D�<�D���D�� D��fD�6fDڌ�D��fD���D�6fD�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�\)@�\)A!G�AAG�AaG�A���A��
A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�\)B���B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C.C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
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
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDtXRDx��D��D�E�D�u�D�ҏD�\D�H�D���D��)D�)D�?\D��)D�D���D�8�Dڏ\D���D��)D�8�D�)D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��`A��TA��yA��A��A��A��yA��mA��;A���AЗ�A�VA�ZA�r�AЏ\AЩ�AЧ�AХ�AН�AЙ�AГuAЉ7A�|�A�r�A�l�A�bNA�\)A�\)A�ZA�M�A�E�A�;dA�;dA�;dA�;dA�;dA�9XA��A���Aɲ-A� �A�G�A�XAģ�A���A��#A��9A�K�A�ĜA�1A��A�{A��A�r�A���A���A�=qA��7A�-A���A��A��9A�ȴA�l�A�r�A��RA���A���A�A�A��A�Q�A���A�1A��PA��/A�C�A��PA�oA��/A�bNA�ZA��wA� �A�A�A��A���A�1A���A��A�(�A�-A���A��A�`BA�bNA�\)A��#A�p�A��A��9A��DA�
=A��/A�ƨA�/A�&�A�5?A��A}K�A{+Az��Ay��Awp�Ar��Aol�Al�!Ai��Agt�Acx�A^�A\I�A[�;AY��AX(�AWS�AVM�AT��AR�uAO�AMhsAK�AJ��AH�uAH  AE�#AC&�A@��A?XA>  A=�A<M�A:v�A8z�A6E�A5?}A4��A4�\A4jA3\)A1��A0�/A0�A/p�A.�HA-�A-?}A,Q�A*��A(��A)+A(�HA(�uA(1'A&��A%�A#�-A" �A"��A!�A!��A!l�A ȴA 5?A��A��A��A�A�-A��A%A�AJA`BA�/An�A�#AhsA�A
r�A	�hA	XA	33A	oA�+AZAr�A��A��A
1'A
5?A5?A?}AVAz�A%A��A%A%A�AVA�-AC�Ap�@��;@�t�@��y@�@���@���@��T@���@��
@��j@�V@�Ĝ@��@��@�\@�O�@���@�v�@� �@���@ݲ-@�x�@ܣ�@�(�@�C�@�E�@�%@�&�@ؼj@׍P@�hs@Ӿw@Ӿw@ӥ�@��@Гu@�n�@���@�z�@�
=@ɲ-@���@ɑh@Ǖ�@å�@���@�Q�@��@�\)@�ȴ@�M�@���@�Q�@�r�@�`B@��@��T@�%@���@���@�/@��@��@�V@��;@���@�V@��T@�Q�@�A�@��m@�^5@���@���@�;d@�ff@�M�@�o@���@��\@��m@�ƨ@��H@�b@�33@���@�33@��-@���@���@�p�@���@���@��^@��#@���@��^@�@��7@�X@��@��@��^@��T@���@�?}@�r�@��w@�1@�o@��@�?}@��@�^5@�1@�l�@�t�@��@�C�@��!@�Z@���@�b@�9X@�K�@��@��@��D@���@��@�dZ@�+@���@�^5@��@�X@���@��u@�r�@�j@�7L@���@�\)@��H@�V@���@�1'@�dZ@��R@�V@�=q@�@���@��;@��P@�ƨ@�o@���@�/@�bN@��;@���@���@�j@��@��7@��@���@�n�@�^5@���@�n�@�{@���@��9@���@�r�@�  @��
@�Q�@�&�@���@���@�~�@�~�@�ff@�{@���@�hs@��@���@���@���@��;@�S�@��!@��!@�{@�-@��h@�r�@���@���@�{@�x�@��j@�1'@�ƨ@��@�9X@��j@���@��D@�Z@� �@�ƨ@��F@���@�l�@�\)@�C�@�33@���@��R@�M�@��@�x�@�/@��@�`B@�?}@�/@��@�r�@�9X@�1'@�1'@� �@�1@��;@��@�l�@���@��R@�~�@��@�?}@��/@�V@��D@�(�@�1@���@���@�1@�  @��@�b@�b@��w@�ȴ@��+@�n�@�J@���@��@��@���@��^@��^@�@��-@���@�@�r�@y7L@mV@c"�@Z�!@T�@I��@BM�@;��@4�j@.��@(�9@%p�@!�@��@�9@�@�;@�@	&�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��`A��`A��TA��yA��A��A��A��yA��mA��;A���AЗ�A�VA�ZA�r�AЏ\AЩ�AЧ�AХ�AН�AЙ�AГuAЉ7A�|�A�r�A�l�A�bNA�\)A�\)A�ZA�M�A�E�A�;dA�;dA�;dA�;dA�;dA�9XA��A���Aɲ-A� �A�G�A�XAģ�A���A��#A��9A�K�A�ĜA�1A��A�{A��A�r�A���A���A�=qA��7A�-A���A��A��9A�ȴA�l�A�r�A��RA���A���A�A�A��A�Q�A���A�1A��PA��/A�C�A��PA�oA��/A�bNA�ZA��wA� �A�A�A��A���A�1A���A��A�(�A�-A���A��A�`BA�bNA�\)A��#A�p�A��A��9A��DA�
=A��/A�ƨA�/A�&�A�5?A��A}K�A{+Az��Ay��Awp�Ar��Aol�Al�!Ai��Agt�Acx�A^�A\I�A[�;AY��AX(�AWS�AVM�AT��AR�uAO�AMhsAK�AJ��AH�uAH  AE�#AC&�A@��A?XA>  A=�A<M�A:v�A8z�A6E�A5?}A4��A4�\A4jA3\)A1��A0�/A0�A/p�A.�HA-�A-?}A,Q�A*��A(��A)+A(�HA(�uA(1'A&��A%�A#�-A" �A"��A!�A!��A!l�A ȴA 5?A��A��A��A�A�-A��A%A�AJA`BA�/An�A�#AhsA�A
r�A	�hA	XA	33A	oA�+AZAr�A��A��A
1'A
5?A5?A?}AVAz�A%A��A%A%A�AVA�-AC�Ap�@��;@�t�@��y@�@���@���@��T@���@��
@��j@�V@�Ĝ@��@��@�\@�O�@���@�v�@� �@���@ݲ-@�x�@ܣ�@�(�@�C�@�E�@�%@�&�@ؼj@׍P@�hs@Ӿw@Ӿw@ӥ�@��@Гu@�n�@���@�z�@�
=@ɲ-@���@ɑh@Ǖ�@å�@���@�Q�@��@�\)@�ȴ@�M�@���@�Q�@�r�@�`B@��@��T@�%@���@���@�/@��@��@�V@��;@���@�V@��T@�Q�@�A�@��m@�^5@���@���@�;d@�ff@�M�@�o@���@��\@��m@�ƨ@��H@�b@�33@���@�33@��-@���@���@�p�@���@���@��^@��#@���@��^@�@��7@�X@��@��@��^@��T@���@�?}@�r�@��w@�1@�o@��@�?}@��@�^5@�1@�l�@�t�@��@�C�@��!@�Z@���@�b@�9X@�K�@��@��@��D@���@��@�dZ@�+@���@�^5@��@�X@���@��u@�r�@�j@�7L@���@�\)@��H@�V@���@�1'@�dZ@��R@�V@�=q@�@���@��;@��P@�ƨ@�o@���@�/@�bN@��;@���@���@�j@��@��7@��@���@�n�@�^5@���@�n�@�{@���@��9@���@�r�@�  @��
@�Q�@�&�@���@���@�~�@�~�@�ff@�{@���@�hs@��@���@���@���@��;@�S�@��!@��!@�{@�-@��h@�r�@���@���@�{@�x�@��j@�1'@�ƨ@��@�9X@��j@���@��D@�Z@� �@�ƨ@��F@���@�l�@�\)@�C�@�33@���@��R@�M�@��@�x�@�/@��@�`B@�?}@�/@��@�r�@�9X@�1'@�1'@� �@�1@��;@��@�l�@���@��R@�~�@��@�?}@��/@�V@��D@�(�@�1@���@���@�1@�  @��@�b@�b@��w@�ȴ@��+@�n�@�J@���@��@��@���@��^@��^@�@��-@���G�O�@�r�@y7L@mV@c"�@Z�!@T�@I��@BM�@;��@4�j@.��@(�9@%p�@!�@��@�9@�@�;@�@	&�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
DB
:^B
�B
��B
��B�B"�B&�B'�B(�B'�B'�B'�B&�B%�B$�B"�B!�B!�B"�B!�B"�B"�B"�B"�B"�B"�B#�B%�BS�BT�BhsB�?B�B��B!�BD�BaHBp�B}�B�B�B��B��B�XB�jB�#B�ZB�mB�sB�sB�TB�fB�B��B��B��B�B�B�B�B�B�HBŢB�LB��B�JBgmBL�B=qB2-B"�B�BbB	7BB�BŢB�B�bBr�BD�B;dB0!B%�B�BB
��B
�B
�B
�`B
�BB
��B
�qB
��B
�B
`BB
J�B
1'B

=B	��B	�B	�fB	��B	�!B	��B	�1B	w�B	ffB	O�B	9XB	,B	'�B	�B	�B	uB	PB	B��B�`B�B��BǮB�wB�^B�'B��B��B��B�uB�PB�=B�%B�B�+B�\B�oB�uB�{B�oB�PB�JB�VB�hB�bB�hB��B��B��B�3BǮB��B��B��B��B��B��B��B�B�/B�BB�;B�mB�`B�BĜB�B��B� Bn�Be`Be`Be`BcTBbNB`BB`BB`BB_;B_;BbNBffBm�Br�Bz�B�B�bB��B��B�FBȴB�XB�-B�B�FBB��B��B�B��B��B��B��B��B��B��B��B��B��B�B�dB�dB�LB�B��B��B��B�PB�B� B}�B�B�B�B�DB�DB�=B�DB�DB�JB�VB��B��B��B��B��B��B��B��B��B��B�B�!B�9B�?B�?B�9B�-B��B��B��B��B��B��B��B��B��B�B�?B��BĜB��B�wB�wB�}B��B��BĜBȴBɺB��B��B�B�)B�;B�`B��B	1B		7B	VB	{B	�B	.B	5?B	>wB	D�B	W
B	cTB	bNB	ffB	hsB	p�B	x�B	x�B	{�B	�B	�B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�!B	�FB	�FB	�XB	�dB	�RB	�9B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�FB	�XB	�FB	�?B	�-B	�!B	�!B	�'B	�'B	�'B	�'B	�?B	�FB	�?B	�?B	�?B	�FB	�XB	�wB	�qB	�jB	�wB	�qB	�qB	�RB	�?B	�FB	�XB	�XB	�LB	�9B	�9B	�LB	�dB	�^B	�RB	�9B	�'B	�!B	�!B	�-B	�jB	ĜB	ŢB	ĜB	B	�}B	B	ȴB	ɺB	ɺB	ȴB	ǮB	ǮB	ƨB	ƨB	ȴB	��B	�B	�5B	�HB	�HB	�HB	�TB	�ZB	�ZB	�TB	�TB	�ZB	�ZB	�`B	�TB	�TB	�ZB	�mB	�sB	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
+B
	7B
	7B
+B
+B
1B
1B
	7B
DB
DB
DB
DB
DB
JB
JB
PB
uB
bB
�B
�B
+B
.B
2-B
=qB
C�B
I�B
O�B
S�B
ZB
^5B
bNB
e`B
hsB
l�B
q�B
v�B
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
FB
:^B
�B
��B
��B{B"�B&�B'�B(�B'�B'�B'�B&�B%�B$�B"�B!�B!�B"�B!�B"�B"�B"�B"�B"�B"�B#�B%�BS�BT�BhnB�6B�B��B!�BD�Ba@Bp�B}�B�B�B��B��B�SB�dB�B�SB�gB�mB�pB�OB�bB�B��B��B��B�B�B�B�B�xB�DBŝB�EB��B�BBggBL�B=jB2(B"�B�B\B	.B �B�BřB�B�[Br�BD�B;^B0B%�B|BB
��B
�B
�B
�ZB
�=B
��B
�lB
��B
�B
`<B
J�B
1%B

=B	��B	�B	�eB	��B	�#B	��B	�4B	w�B	fkB	O�B	9]B	,B	'�B	�B	�B	|B	VB	B��B�jB�B��BǷB��B�jB�3B��B��B��B��B�]B�KB�1B�B�8B�iB�}B��B��B�|B�`B�XB�cB�wB�nB�uB��B��B��B�?BǸB��B��B��B�B��B��B��B�&B�8B�LB�DB�tB�gB�%BĥB�&B��B�Bn�BemBenBelBcbBb[B`QB`QB`MB_HB_JBb\BfvBm�Br�Bz�B�B�oB��B��B�OBȽB�aB�9B�B�PBB��B��B�%B��B��B��B��B��B��B��B��B��B��B�B�mB�iB�UB�#B��B��B��B�[B�&B�B}�B�B�*B�B�PB�NB�HB�PB�OB�UB�_B��B��B��B��B��B��B��B��B��B��B�B�*B�CB�JB�GB�?B�5B��B��B��B��B��B��B��B��B��B�
B�HB��BĥB��B�B�~B��B��B��BģBȺB��B��B��B�B�.B�BB�gB��B	5B		=B	[B	B	�B	.B	5BB	>zB	D�B	WB	cTB	bMB	fgB	hvB	p�B	x�B	x�B	{�B	�B	�B	�bB	�{B	�B	��B	��B	��B	��B	��B	��B	�B	�&B	�"B	�CB	�EB	�VB	�dB	�QB	�7B	�+B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�7B	�CB	�WB	�DB	�>B	�+B	� B	� B	�&B	�'B	�(B	�%B	�>B	�DB	�<B	�;B	�<B	�FB	�UB	�wB	�pB	�eB	�uB	�oB	�oB	�PB	�=B	�DB	�UB	�VB	�LB	�7B	�6B	�GB	�dB	�\B	�OB	�9B	�%B	� B	�B	�+B	�iB	ĜB	šB	ĚB	B	�zB	B	ȰB	ɹB	ɸB	ȲB	ǮB	ǯB	ƦB	ƤB	ȱB	��B	� B	�1B	�DB	�BB	�FB	�RB	�VB	�UB	�PB	�SB	�VB	�WB	�^B	�PB	�PB	�XB	�kB	�pB	�B	�B	�B	�B	�vB	�pB	�qB	�qB	�wB	�xB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B

B
B
B
B
)B
	2B
	1B
(B
'B
*B
+B
	1B
=B
>B
>B
>B
?B
GB
GB
LG�O�B
^B
�B
�B
*�B
.B
2)B
=kB
C�B
I�B
O�B
S�B
ZB
^/B
bIB
eYB
hkB
l�B
q�B
v�B
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436412016080714364120160807143641  AO  ARCAADJP                                                                    20151210101643    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151210101643  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151210101643  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143641  IP                  G�O�G�O�G�O�                