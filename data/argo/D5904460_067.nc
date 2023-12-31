CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-14T02:16:07Z AOML 3.0 creation; 2016-08-07T21:17:39Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150814021607  20160807141739  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               CA   AO  5285_8895_067                   2C  D   APEX                            6487                            072314                          846 @�gr|� 
1   @�gsF?�@+�=p��
�c�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    CA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B ffB'��B/��B7��B@  BH  BP  BX  B`  Bh  BpffBx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C33C  C  C�fC	�fC�fC  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dx��D�	�D�C3D��fD�ɚD��fD�I�D�� D�ٚD��D�P D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�z�A
=qA*=qAJ=qAj=qA��A��A��A��A��A��A��A��B�\B
�\B\)B(�B"��B*(�B2(�B:(�BB�\BJ�\BR�\BZ�\Bb�\Bj�\Br��Bz�\B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�z�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C �qC�
C��C��C�=C
�=C�=C��C��C��C��C��C��C��C��C�=C ��C"��C$��C&��C(�qC*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D	(�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'(�D'��D((�D(��D)(�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3(�D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<(�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD(�DD��DE(�DE��DF(�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR�\DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX��DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db(�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk(�Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt��Dy�D�D�W�D���D��D���D�^D��{D��D�.D�d{D��{D�׮D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�$�A���A�ĜA�A�p�A�dZA�ZA�XA�VA�G�A�A�A�A�A�=qA�5?A�1'A� �A�33A��A�x�A�`BA�%A�^5A���AΑhA͕�A�O�A̼jAʸRAȝ�A�33AƼjA�z�A�K�A�$�A�ƨA�r�A�VA�+A§�A�A�(�A���A��A���A�M�A���A��9A��;A��A�=qA��A���A�jA�JA��yA�z�A�~�A�bNA�{A�O�A�VA��jA�K�A�S�A�A��A�?}A�z�A�
=A�=qA�~�A|��Az~�Aw�Ar=qAn��AlĜAj��AhVAd�AbI�Aa�A`1A^�HAWdZATĜAR��AM�AI�AH��AE�AD$�AB��AA33A@9XA?`BA>�`A>E�A<��A:�A8Q�A77LA5��A5p�A5oA3�A3oA2ffA0�A,��A(��A'p�A'hsA'�^A&�A%�-A%O�A$��A#��A"�HA!��A!�hA!XA ��A ��A"(�A"  A �A   A��A`BAdZAXAĜAA�AI�A�A�`A��AVA�
A��Az�A �A9XA-A��AC�A�A�DA�A�7AhsA7LA%A�A��Ar�AA�A�AVA��A��AdZA7LA7LAXA��A��A^5AS�A��A5?A��A��AC�A/A
�HA
��A
��A
=qA	�#A	�wA	�^A	�AZAAp�AoAA��A��A��AVA �A1'A�A7LAȴAv�AM�Al�A��AA�A-A�-A|�A+A"�A v�A @��@�x�@��9@�"�@���@��@�Q�@�b@���@���@���@�x�@�Ĝ@�D@�Z@�|�@��@���@�`B@���@���@�1'@�"�@�@�R@�~�@���@���@�  @�t�@�@��@�E�@�7L@�Ĝ@��@�Z@��m@��
@�|�@��@���@�=q@�-@�u@�w@�ȴ@�ff@��@��@�7L@���@�A�@��m@߶F@�t�@���@�E�@�?}@�r�@�b@��m@ۥ�@�S�@�@�^5@��@�O�@�Ĝ@�A�@��m@�|�@���@�~�@�^5@�5?@�@Չ7@�`B@� �@�
=@ҏ\@�@���@�z�@�1'@�  @��@�n�@�J@́@�G�@��/@�(�@�b@�|�@�"�@�ȴ@�$�@ɡ�@Ɂ@��@���@ȃ@�(�@�|�@��@���@�n�@��@���@�&�@�A�@�1@Ý�@�t�@�33@�ȴ@�ff@�=q@�$�@���@�`B@�Z@�b@���@�33@�E�@��#@�x�@��@���@���@��/@���@�Ĝ@�z�@�ƨ@�C�@�"�@�@��@�$�@���@���@�x�@�&�@�Ĝ@�z�@�I�@��;@�l�@�@���@�$�@�hs@�V@�z�@� �@���@���@�S�@���@�n�@�E�@�J@�@��h@�`B@��@��/@�bN@�(�@��;@��P@�@��+@�V@�{@��@���@���@�X@���@��/@�9X@�  @�l�@��@��R@�E�@��T@���@�&�@�Ĝ@�r�@�9X@�b@��w@�K�@��H@�v�@�E�@��T@�hs@��@�%@��@�b@�  @�b@�ƨ@�
=@���@�V@��@��@���@��@���@�z�@�A�@���@��P@�;d@��H@�V@��7@��@��@�1'@��w@��@�+@���@��+@���@��^@�7L@���@���@� �@��;@�;d@���@���@�V@���@���@���@�X@��@�Ĝ@���@�1'@�1@���@���@�C�@���@��!@��+@�n�@�-@��T@��@�&�@��/@�bN@�9X@�(�@��@��F@�dZ@�S�@�o@���@��@��^@yX@n{@f��@]�h@T�@M�T@F5?@?��@7�w@2=q@,��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�(�A�$�A���A�ĜA�A�p�A�dZA�ZA�XA�VA�G�A�A�A�A�A�=qA�5?A�1'A� �A�33A��A�x�A�`BA�%A�^5A���AΑhA͕�A�O�A̼jAʸRAȝ�A�33AƼjA�z�A�K�A�$�A�ƨA�r�A�VA�+A§�A�A�(�A���A��A���A�M�A���A��9A��;A��A�=qA��A���A�jA�JA��yA�z�A�~�A�bNA�{A�O�A�VA��jA�K�A�S�A�A��A�?}A�z�A�
=A�=qA�~�A|��Az~�Aw�Ar=qAn��AlĜAj��AhVAd�AbI�Aa�A`1A^�HAWdZATĜAR��AM�AI�AH��AE�AD$�AB��AA33A@9XA?`BA>�`A>E�A<��A:�A8Q�A77LA5��A5p�A5oA3�A3oA2ffA0�A,��A(��A'p�A'hsA'�^A&�A%�-A%O�A$��A#��A"�HA!��A!�hA!XA ��A ��A"(�A"  A �A   A��A`BAdZAXAĜAA�AI�A�A�`A��AVA�
A��Az�A �A9XA-A��AC�A�A�DA�A�7AhsA7LA%A�A��Ar�AA�A�AVA��A��AdZA7LA7LAXA��A��A^5AS�A��A5?A��A��AC�A/A
�HA
��A
��A
=qA	�#A	�wA	�^A	�AZAAp�AoAA��A��A��AVA �A1'A�A7LAȴAv�AM�Al�A��AA�A-A�-A|�A+A"�A v�A @��@�x�@��9@�"�@���@��@�Q�@�b@���@���@���@�x�@�Ĝ@�D@�Z@�|�@��@���@�`B@���@���@�1'@�"�@�@�R@�~�@���@���@�  @�t�@�@��@�E�@�7L@�Ĝ@��@�Z@��m@��
@�|�@��@���@�=q@�-@�u@�w@�ȴ@�ff@��@��@�7L@���@�A�@��m@߶F@�t�@���@�E�@�?}@�r�@�b@��m@ۥ�@�S�@�@�^5@��@�O�@�Ĝ@�A�@��m@�|�@���@�~�@�^5@�5?@�@Չ7@�`B@� �@�
=@ҏ\@�@���@�z�@�1'@�  @��@�n�@�J@́@�G�@��/@�(�@�b@�|�@�"�@�ȴ@�$�@ɡ�@Ɂ@��@���@ȃ@�(�@�|�@��@���@�n�@��@���@�&�@�A�@�1@Ý�@�t�@�33@�ȴ@�ff@�=q@�$�@���@�`B@�Z@�b@���@�33@�E�@��#@�x�@��@���@���@��/@���@�Ĝ@�z�@�ƨ@�C�@�"�@�@��@�$�@���@���@�x�@�&�@�Ĝ@�z�@�I�@��;@�l�@�@���@�$�@�hs@�V@�z�@� �@���@���@�S�@���@�n�@�E�@�J@�@��h@�`B@��@��/@�bN@�(�@��;@��P@�@��+@�V@�{@��@���@���@�X@���@��/@�9X@�  @�l�@��@��R@�E�@��T@���@�&�@�Ĝ@�r�@�9X@�b@��w@�K�@��H@�v�@�E�@��T@�hs@��@�%@��@�b@�  @�b@�ƨ@�
=@���@�V@��@��@���@��@���@�z�@�A�@���@��P@�;d@��H@�V@��7@��@��@�1'@��w@��@�+@���@��+@���@��^@�7L@���@���@� �@��;@�;d@���@���@�V@���@���@���@�X@��@�Ĝ@���@�1'@�1@���@���@�C�@���@��!@��+@�n�@�-@��T@��@�&�@��/@�bN@�9X@�(�@��@��F@�dZ@�S�G�O�@���@��@��^@yX@n{@f��@]�h@T�@M�T@F5?@?��@7�w@2=q@,��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	�B	��B	��B	��B	�'B	�5B
uB
�B
\B
%B
VB
�B
��B
�sBo�B��B�BN�Bz�B�\B��B��B��B�B��B��B�B�B�B�?B{�BoB�B�B�3B��B�VBZB'�BJB  B
�sB
��B
�B
�oB
w�B
YB
%�B

=B	��B	��B	�B	��B	��B	�hB	�B	o�B	XB	E�B	6FB	/B	%�B	{B		7B	B	B��B�)B�
B��B��B��B��B��B��B�B�
B�B�#B�)B�)B�BB�sB��B	B	hB	�B	�B	(�B	8RB	A�B	J�B	F�B	6FB	6FB	?}B	R�B	YB	\)B	n�B	jB	ffB	jB	m�B	l�B	jB	ffB	z�B	��B	�FB	�dB	�LB	��B	ŢB	ƨB	ǮB	ǮB	ĜB	ƨB	ÖB	�qB	�LB	�B	�B	��B	��B	��B	�B	�3B	�9B	�9B	�3B	�jB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	�#B	�B	�#B	�BB	�NB	�TB	�TB	�`B	�B	�B	�B	�B	�`B	�NB	�ZB	�ZB	�fB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�mB	�sB	�mB	�mB	�mB	�yB	�B	�B	�yB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
	7B
	7B
1B
1B
1B
1B
	7B
	7B
DB
DB

=B
DB
DB
DB
DB

=B
DB
DB
DB
DB
PB
PB
PB
VB
VB
bB
bB
bB
hB
bB
hB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
$�B
#�B
$�B
'�B
&�B
.B
49B
<jB
B�B
G�B
L�B
P�B
VB
ZB
^5B
cTB
gmB
iy1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	�B	�B	�B	�B	{B	tB	pB	hB	nB	nB	hB	iB	oB	oB	jB	pB	�B	#�B	��B	�ZB	��B	��B	��B	�B
JB
sB
3B
�B
U�B
��B
��B
�DBojB��BYBN�Bz�B�&B��B��B��B��B��B��B��B��B��B�	B{�B7B�WB��B��B��B�BY�B'�BB
��B
�:B
͚B
��B
�6B
w�B
X�B
%�B

B	��B	�B	��B	�LB	��B	�2B	��B	okB	W�B	EoB	6B	.�B	%�B	GB		B	�B	 �B��B��B��BΦBʑB˓B˓B��B��B��B��B��B��B��B��B�B�=B��B	�B	0B	IB	gB	(�B	8B	AOB	J�B	FmB	6B	6	B	?@B	R�B	X�B	[�B	nYB	jAB	f&B	jBB	mRB	lMB	j@B	f&B	z�B	��B	�B	�"B	�B	�FB	�_B	�fB	�lB	�nB	�WB	�dB	�RB	�/B	�	B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	�(B	�ZB	�jB	͐B	ϚB	ϚB	СB	ѩB	ԹB	��B	��B	��B	��B	�	B	�B	�B	�B	�;B	�KB	�LB	�AB	�B	�B	�B	�B	� B	�"B	�9B	�>B	�GB	�_B	�rB	�zB	��B	��B	�}B	�uB	�oB	�cB	�]B	�XB	�VB	�]B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	�gB	��B	��B	��B	��B	�vB	�dB	�>B	�&B	�,B	�'B	�'B	�#B	�1B	�5B	�5B	�0B	�0B	�/B	�6B	�1B	�0B	�;B	�IB	�OB	�CB	�DB	�GB	�EB	�?B	�7B	�+B	�B	�'B	�$B	�2B	�0B	�4B	�<B	�=B	�>B	�>B	�>B	�6B	�1B	�>B	�AB	�CB	�;B	�>B	�9B	�1B	�/B	�5B	�=B	�:B	�AB	�HB	�MB	�PB	�GB	�HB	�MB	�PB	�VB	�\B	�eB	�fB	�eB	�ZB	�TB	�VB	�ZB	�YB	�ZB	�]B	�^B	�bB	�_B	�cB	�_B	�_B	�YB	�[B	�ZB	�UB	�TB	�RB	�NB	�MB	�OB	�RB	�NB	�MB	�MB	�MB	�HB	�GB	�AB	�AB	�GB	�KB	�HB	�MB	�EB	�OB	�TB	�XB	�ZB	�_B	�aB	�_B	�`B	�^B	�[B	�ZB	�^B	�dB	�fB	�fB	�eB	�mB	�pB	�qB	�pB	�lB	�qB	�qB	�rB	�lB	�kB	�pB	�xB	�xB	��B	�|B	�B	��B	�}B	�}B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

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
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
!B
$B
!B
(B
*B
)B
+B
&B
*B
,B
-B
3B
<B
<B
?B
?B
AB
AB
AB
HB
HB
DB
HB
MB
RB
JB
LB
QB
]B
_B
fB
iB
qB
qB
 wB
 xB
 zB
 xB
 xB
 xB
 xB
 wB
 xB
 vB
!{B
"�B
#�B
#�B
$�B
#�B
$�G�O�B
&�B
-�B
3�B
<B
B@B
G_B
L~B
P�B
U�B
Y�B
]�B
cB
gB
i+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.64 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417392016080714173920160807141739  AO  ARCAADJP                                                                    20150814021607    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150814021607  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150814021607  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141739  IP                  G�O�G�O�G�O�                