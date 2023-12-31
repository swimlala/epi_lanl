CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:52Z AOML 3.0 creation; 2016-08-07T21:17:31Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221252  20160807141731  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_015                   2C  D   APEX                            6487                            072314                          846 @�#U}� 1   @�#VF?�@,%`A�7L�c��S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi��Bp  Bv  B�  B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D��D�Y�D�� D��fD�	�D�@ D���D���D�fD�S3D��3Dǣ3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�z�A
=qA*=qAJ=qAj=qA��A��A��A��A��A��A��A��B�\B
�\B�\B�\B"�\B*�\B2�\B:�\BB�\BJ�\BR�\BZ�\Bb�\Bl(�Br�\Bx�\B�G�B�z�B�{B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C ��C��C�=C��C��C
�qC��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&�qC(��C*��C,�=C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�EC�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�EC�EC�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D	(�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'(�D'��D((�D(��D)(�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3(�D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<(�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD(�DD��DE(�DE��DF(�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR��DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX��DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db(�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk(�Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt�\Dy��D�.D�nD��{D���D�D�T{D��HD��D��D�g�D���DǷ�D�.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aۡ�Aۣ�Aۡ�A�I�A�JAډ7A�jA�5?A��A�%A���A��yA��;A��
A���A���Aٺ^Aٰ!A٬A٬A١�Aٙ�Aُ\AٍPAه+AفA�z�A�1'A��A�A�A�t�A� �Aĥ�A�ĜA�-A���A�/A�?}A���A�-A�z�A�A��;A�{A� �A�XA��A�v�A�?}A��A��`A�XA��A���A��HA�/A���A�1'A��/A��wA�+A��7A��FA���A��+A���A��\A�ffA��A�"�A��yA��
A�&�A��A��A��#A}��Aw"�Aq;dAl��Ae��Aa+A\1AX(�AT$�AO�7AM�7ALbNAK�mAJ~�AG�wAEAB �A?�mA=�A=oA<��A<(�A:�A8I�A61A3dZA1�PA/��A.=qA,5?A)�A'��A&�HA&v�A%�A$n�A#33A"��A"(�A#�A �HA �A�FA�TA1AVA�mA 9XA#|�A#?}A v�Av�An�A��Al�A/A1'A�wA�A;dA��A  A33A{A��A�yA�jA�^A�TA��AG�AoA(�A(�A�A�A�7Av�A9XA�Av�AbA��AĜA�A  A��A�#A$�A�A�-A?}A��AVA5?A1A�AAƨA�AVA|�A9XA��A�A�hA�wAA��A
��A
��A
~�A
I�A
��A
�9A	K�A�/A�/A��A�A�A�^AC�A��AI�A`BA�9A^5A  A��A%A1A33A �9@�ff@�7L@��9@��D@�Q�@�l�@���@�n�@�ff@�x�@��@�-@��@�r�@�bN@�Q�@�1@�@�@�M�@�`B@��@�Q�@��
@�@�33@���@��@�9@�\)@�=q@�@�x�@�%@�j@畁@�33@�K�@��
@�p�@�?}@���@�@�ƨ@�\@�^@�r�@㝲@���@�+@�E�@�$�@�J@��#@�h@�@�O�@��u@�9X@ߕ�@ް!@ݲ-@�1@���@�J@�hs@�bN@ץ�@�v�@��@Չ7@��@���@���@���@���@�1'@ҟ�@�^5@�=q@�{@���@��@���@��@��m@ϝ�@�S�@�"�@�@�v�@�n�@�J@��`@̴9@���@��;@�1@�ƨ@ˍP@�\)@���@�v�@�^5@���@�$�@��@�$�@�O�@Ȭ@��@�-@��@���@�O�@�j@å�@��@§�@�=q@���@�G�@���@��j@�Q�@� �@��@�l�@�
=@��@���@�M�@�J@��@���@��7@�O�@��@�Ĝ@��;@�t�@�\)@�+@�ff@��@�@�&�@�9X@�  @�33@���@�X@�?}@��@���@��/@�Ĝ@��j@�Q�@�(�@�Q�@�A�@���@��@�ff@�M�@���@�X@��;@���@��@��\@��+@�n�@�E�@��T@���@�p�@���@�1'@�  @���@���@�\)@�
=@���@�M�@�J@���@���@��D@�1@��F@���@��P@�C�@�o@��H@���@��R@��\@�n�@��@��^@�hs@�&�@���@���@�bN@�A�@�ƨ@�33@�=q@�@��^@�G�@��`@�j@��@�|�@�o@��+@��@�@�`B@�&�@�V@��@��j@��D@�bN@�b@���@���@�t�@�K�@�"�@���@��H@���@�n�@���@��^@�p�@�G�@���@��@�A�@��
@��@�\)@�33@��y@��\@�ff@�M�@�E�@�=q@�5?@��@��h@�/@��@�%@���@�j@� �@���@���@�l�@�33@�"�@��@���@�-@���@�O�@��@��H@��+@��h@~�R@vȴ@l�/@c�m@Yhs@So@G|�@@ �@8�`@1��@,�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  Aۡ�Aۣ�Aۡ�A�I�A�JAډ7A�jA�5?A��A�%A���A��yA��;A��
A���A���Aٺ^Aٰ!A٬A٬A١�Aٙ�Aُ\AٍPAه+AفA�z�A�1'A��A�A�A�t�A� �Aĥ�A�ĜA�-A���A�/A�?}A���A�-A�z�A�A��;A�{A� �A�XA��A�v�A�?}A��A��`A�XA��A���A��HA�/A���A�1'A��/A��wA�+A��7A��FA���A��+A���A��\A�ffA��A�"�A��yA��
A�&�A��A��A��#A}��Aw"�Aq;dAl��Ae��Aa+A\1AX(�AT$�AO�7AM�7ALbNAK�mAJ~�AG�wAEAB �A?�mA=�A=oA<��A<(�A:�A8I�A61A3dZA1�PA/��A.=qA,5?A)�A'��A&�HA&v�A%�A$n�A#33A"��A"(�A#�A �HA �A�FA�TA1AVA�mA 9XA#|�A#?}A v�Av�An�A��Al�A/A1'A�wA�A;dA��A  A33A{A��A�yA�jA�^A�TA��AG�AoA(�A(�A�A�A�7Av�A9XA�Av�AbA��AĜA�A  A��A�#A$�A�A�-A?}A��AVA5?A1A�AAƨA�AVA|�A9XA��A�A�hA�wAA��A
��A
��A
~�A
I�A
��A
�9A	K�A�/A�/A��A�A�A�^AC�A��AI�A`BA�9A^5A  A��A%A1A33A �9@�ff@�7L@��9@��D@�Q�@�l�@���@�n�@�ff@�x�@��@�-@��@�r�@�bN@�Q�@�1@�@�@�M�@�`B@��@�Q�@��
@�@�33@���@��@�9@�\)@�=q@�@�x�@�%@�j@畁@�33@�K�@��
@�p�@�?}@���@�@�ƨ@�\@�^@�r�@㝲@���@�+@�E�@�$�@�J@��#@�h@�@�O�@��u@�9X@ߕ�@ް!@ݲ-@�1@���@�J@�hs@�bN@ץ�@�v�@��@Չ7@��@���@���@���@���@�1'@ҟ�@�^5@�=q@�{@���@��@���@��@��m@ϝ�@�S�@�"�@�@�v�@�n�@�J@��`@̴9@���@��;@�1@�ƨ@ˍP@�\)@���@�v�@�^5@���@�$�@��@�$�@�O�@Ȭ@��@�-@��@���@�O�@�j@å�@��@§�@�=q@���@�G�@���@��j@�Q�@� �@��@�l�@�
=@��@���@�M�@�J@��@���@��7@�O�@��@�Ĝ@��;@�t�@�\)@�+@�ff@��@�@�&�@�9X@�  @�33@���@�X@�?}@��@���@��/@�Ĝ@��j@�Q�@�(�@�Q�@�A�@���@��@�ff@�M�@���@�X@��;@���@��@��\@��+@�n�@�E�@��T@���@�p�@���@�1'@�  @���@���@�\)@�
=@���@�M�@�J@���@���@��D@�1@��F@���@��P@�C�@�o@��H@���@��R@��\@�n�@��@��^@�hs@�&�@���@���@�bN@�A�@�ƨ@�33@�=q@�@��^@�G�@��`@�j@��@�|�@�o@��+@��@�@�`B@�&�@�V@��@��j@��D@�bN@�b@���@���@�t�@�K�@�"�@���@��H@���@�n�@���@��^@�p�@�G�@���@��@�A�@��
@��@�\)@�33@��y@��\@�ff@�M�@�E�@�=q@�5?@��@��h@�/@��@�%@���@�j@� �@���@���@�l�@�33@�"�@��@���@�-@���@�O�G�O�@��H@��+@��h@~�R@vȴ@l�/@c�m@Yhs@So@G|�@@ �@8�`@1��@,�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B�B�sB�B.B33B1'B#�BJB�B
=B��B�mB��B�`B0!B+B�B�B�B`BBm�BaHBhsB.BoBVBVBB��B�`B�
B�BĜB��B�7BA�B
��B
��B
{�B
_;B
C�B
<jB
$�B
B	�B	�'B	�oB	l�B	P�B	49B	�B	VB��B��B�B�B�sB�/B��B��BǮBÖBÖB��B�wB�dB�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B�'B�}B�HB��B��B��B�3B�jB�B�B	"�B	jB	o�B	XB	F�B	:^B	7LB	5?B	7LB	;dB	=qB	[#B	m�B	k�B	k�B	�B	�jB	ĜB	ƨB	ƨB	B	�LB	�!B	��B	��B	��B	�B	�!B	�jB	B	�wB	�dB	�LB	�3B	�3B	�9B	�B	��B	�B	�-B	�wB	ƨB	��B	��B	��B	�B	��B	��B	��B	��B	�B	�)B	�mB	�B	�B	�fB	�TB	�TB	�`B	�B	��B	��B	�B	��B	��B	��B

=B
JB
B
B
B
1B

=B
	7B
1B
+B
%B
B	��B	��B	��B	��B	��B	�B	�B	�sB	�TB	�)B	�;B	�`B	�mB	�`B	�TB	�TB	�TB	�sB	�sB	�`B	�ZB	�NB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�NB	�NB	�HB	�BB	�;B	�5B	�5B	�#B	�B	�B	�
B	�B	�B	�B	�#B	�/B	�BB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�sB	�mB	�mB	�mB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�sB	�sB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B	��B	��B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

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
JB
JB
JB
PB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
,B
49B
9XB
?}B
E�B
J�B
Q�B
ZB
_;B
cTB
gmB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B�B�B�yB�aB�RB�6B�7B�[B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B�sB�WB�SB��B�>B]B-�B2�B0�B#�BB�aB
B��B�4B��B�*B/�B*�B}BSBsB`BmVBaBh8B-�B5BBB�B��B�$B��B��B�`B��B��BAMB
��B
��B
{�B
_B
CZB
</B
$�B
�B	��B	��B	�8B	lTB	P�B	4B	�B	!B��B��B�B�iB�>B��BҾB͞B�zB�`B�dB�VB�DB�/B�B��B��B��B��B��B��B�vB��B��B��B��B�}B�vB��B�CB�B̔B��B��B��B�1B��B�QB	"�B	j@B	o`B	W�B	FjB	: B	7B	4�B	7B	;%B	=3B	Z�B	mRB	kDB	kDB	��B	�&B	�ZB	�gB	�dB	�KB	�
B	��B	��B	��B	��B	��B	��B	�&B	�JB	�5B	�"B	�B	��B	��B	��B	��B	��B	��B	��B	�3B	�dB	ΕB	үB	ԺB	տB	ӳB	ҭB	ҮB	ҭB	��B	��B	�(B	�QB	�7B	� B	�B	�B	�B	�PB	�xB	�~B	�QB	��B	��B	��B
	�B
B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	�oB	�IB	�,B	�B	��B	��B	�B	�'B	�B	�B	�B	�B	�-B	�,B	�B	�B	�B	� B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	պB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�{B	�tB	�tB	�fB	�bB	�bB	�`B	�`B	�`B	�aB	�aB	�aB	�aB	�fB	�aB	�TB	�HB	�;B	�4B	�,B	�%B	�B	�B	�B	�#B	�+B	�)B	�+B	�*B	�*B	�#B	�$B	�%B	�1B	�2B	�0B	�.B	�/B	�/B	�/B	�*B	�'B	�$B	�#B	�%B	�$B	�#B	�#B	�#B	�*B	�*B	�+B	�=B	�GB	�DB	�FB	�FB	�OB	�KB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
�B
�B
�B
�B
B
�B
B
B
B

B
B

B

B
B
B
B
B
B
B
B
B
 B
"B
"B
#B
*B
(B
+B
.B
-B
-B
4B
6B
2B
3B
1B
:B
;B
;B
<B
9B
;B
;B
:B
@B
@B
?B
FB
HB
GB
EB
FB
LB
SB
QB
QB
RB
ZB
ZB
[B
WB
YB
ZB
RB
ZB
XB
XB
YB
XB
YB
^B
bB
bB
cB
fB
dB
eB
dB
iB
jB
kG�O�B
qB
#�B
+�B
3�B
9
B
?,B
ETB
JpB
Q�B
Y�B
^�B
cB
gB
j.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.64 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417312016080714173120160807141731  AO  ARCAADJP                                                                    20150226221252    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221252  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221252  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141731  IP                  G�O�G�O�G�O�                