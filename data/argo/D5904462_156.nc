CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:49Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125949  20190405100757  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���nb�1   @������@/��
=p��db��n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D���D�@ D�|�D���D�fD�C3D�s3D��fD� D�@ D�vfD��3D�  D�<�Dڃ3D�fD��D�VfD�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�z�B��HB�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{C �=C�=C�=C�=C�=C
�=C�=C��C��C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��Du�Dy�\D�D�QHD��D���D�'�D�T{D��{D��D�!HD�QHD���D��{D�HD�NDڔ{D�ǮD�.D�g�D��D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A�ƨA���A���A�ĜAة�Aا�A؝�Aؗ�Aؕ�A�p�A�\)A�Q�A�O�A�M�A�M�A�K�A�G�A�E�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�G�A�K�A�I�A�K�A�G�A�;dA�$�A��A�
=A��A�Aե�AԾwA���A�?}A�A��`A��A��A�5?A�  A��`A�(�A��;A���A�{Aď\AÍPA��
A��wA���A�{A�t�A�$�A�r�A���A��A�hsA�33A�^5A�+A��A�n�A��wA�Q�A��/A��mA�ƨA�x�A���A��/A���A�hsA��A���A�&�A��-A���A���A���A�{A��A��FA�
=A��A�%A���A��A�z�A}x�Av��AtAn�AidZAd��Ab-A^^5A[
=AX�jAV^5AU/AR�APr�ANM�AK%AI�PAE��AB��AA%A=dZA;��A:�A9�A8n�A7?}A6�+A6VA6-A5�A4��A3��A2�`A2��A2z�A2 �A1��A0�/A0I�A.��A-x�A,r�A+��A)��A)&�A(^5A'�A&��A&5?A%��A%;dA$�A#��A"��A!�-A �A33AAȴA=qA�A�
A-AdZA�-A
-A	x�AJAA��Ap�A�;AC�A�A��A �u@�
=@�Z@�|�@��@���@���@��@���@��+@�{@�G�@�1'@�\)@�@�@�9@�@�S�@���@��@�@��@��y@��@�A�@���@�C�@���@�{@�^@��#@�7L@�r�@�K�@�R@◍@��y@�S�@�@���@�+@��@�|�@�$�@��@�A�@�Z@�Q�@ߍP@��@�/@�`B@���@�O�@�p�@�O�@�`B@�&�@�&�@�7L@�bN@ܓu@�j@��@�o@�l�@�dZ@�33@�
=@�ȴ@ڧ�@ڇ+@�$�@ّh@��`@�(�@�ƨ@�o@֗�@�E�@�J@���@Ցh@�O�@Լj@�Z@ӥ�@��@ҟ�@�-@�O�@�I�@Ϯ@�\)@θR@���@�&�@��@�A�@˾w@�@��#@���@ȴ9@ț�@�9X@��
@�t�@Ƈ+@�{@�@ũ�@ũ�@���@Ĵ9@ċD@�|�@�S�@��@°!@�^5@�-@§�@�ȴ@°!@°!@���@�;d@�"�@���@§�@�=q@��#@�`B@���@�A�@�l�@�+@�o@�n�@�=q@�@��7@���@�I�@���@�C�@�ȴ@���@�@���@�O�@��9@���@��!@�5?@�@�?}@��/@��@�  @��@�dZ@�+@��@��H@���@�^5@�5?@��#@���@��-@�p�@�&�@���@���@�bN@�(�@��
@��@��@�"�@��+@�v�@�v�@�v�@�n�@�=q@��T@��@�p�@�O�@�&�@�%@��@��/@�Ĝ@���@�z�@�j@�I�@�9X@���@� �@�  @���@���@�l�@���@���@��R@���@�=q@�J@�@��#@��h@�p�@�`B@�/@��@��@��@�Z@�1@���@�|�@�;d@�@��!@�ff@��@��-@�7L@��u@�r�@��F@�|�@�33@�
=@��H@��+@�-@���@��@���@��@�z�@�j@�bN@�I�@���@�\)@�"�@�o@�
=@��@���@�V@��^@�?}@��9@���@�t�@�;d@��H@���@�n�@�=q@�J@��@���@�@���@�X@�Ĝ@�Q�@�1'@��@�b@�1@�  @���@��@��@���@��@���@�K�@���@�^5@���@�X@�/@�Ĝ@�bN@�1'@�b@�  @���@���@�S�@�@���@���@�v�@��@���@���@�x�@�X@�%@�Ĝ@��u@�@�p�@�(�@�@uO�@j-@`�u@W�w@M�@F@>�@5�h@0  @)��@"�@I�@�+@��@/@
�!@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A�ƨA���A���A�ĜAة�Aا�A؝�Aؗ�Aؕ�A�p�A�\)A�Q�A�O�A�M�A�M�A�K�A�G�A�E�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�G�A�K�A�I�A�K�A�G�A�;dA�$�A��A�
=A��A�Aե�AԾwA���A�?}A�A��`A��A��A�5?A�  A��`A�(�A��;A���A�{Aď\AÍPA��
A��wA���A�{A�t�A�$�A�r�A���A��A�hsA�33A�^5A�+A��A�n�A��wA�Q�A��/A��mA�ƨA�x�A���A��/A���A�hsA��A���A�&�A��-A���A���A���A�{A��A��FA�
=A��A�%A���A��A�z�A}x�Av��AtAn�AidZAd��Ab-A^^5A[
=AX�jAV^5AU/AR�APr�ANM�AK%AI�PAE��AB��AA%A=dZA;��A:�A9�A8n�A7?}A6�+A6VA6-A5�A4��A3��A2�`A2��A2z�A2 �A1��A0�/A0I�A.��A-x�A,r�A+��A)��A)&�A(^5A'�A&��A&5?A%��A%;dA$�A#��A"��A!�-A �A33AAȴA=qA�A�
A-AdZA�-A
-A	x�AJAA��Ap�A�;AC�A�A��A �u@�
=@�Z@�|�@��@���@���@��@���@��+@�{@�G�@�1'@�\)@�@�@�9@�@�S�@���@��@�@��@��y@��@�A�@���@�C�@���@�{@�^@��#@�7L@�r�@�K�@�R@◍@��y@�S�@�@���@�+@��@�|�@�$�@��@�A�@�Z@�Q�@ߍP@��@�/@�`B@���@�O�@�p�@�O�@�`B@�&�@�&�@�7L@�bN@ܓu@�j@��@�o@�l�@�dZ@�33@�
=@�ȴ@ڧ�@ڇ+@�$�@ّh@��`@�(�@�ƨ@�o@֗�@�E�@�J@���@Ցh@�O�@Լj@�Z@ӥ�@��@ҟ�@�-@�O�@�I�@Ϯ@�\)@θR@���@�&�@��@�A�@˾w@�@��#@���@ȴ9@ț�@�9X@��
@�t�@Ƈ+@�{@�@ũ�@ũ�@���@Ĵ9@ċD@�|�@�S�@��@°!@�^5@�-@§�@�ȴ@°!@°!@���@�;d@�"�@���@§�@�=q@��#@�`B@���@�A�@�l�@�+@�o@�n�@�=q@�@��7@���@�I�@���@�C�@�ȴ@���@�@���@�O�@��9@���@��!@�5?@�@�?}@��/@��@�  @��@�dZ@�+@��@��H@���@�^5@�5?@��#@���@��-@�p�@�&�@���@���@�bN@�(�@��
@��@��@�"�@��+@�v�@�v�@�v�@�n�@�=q@��T@��@�p�@�O�@�&�@�%@��@��/@�Ĝ@���@�z�@�j@�I�@�9X@���@� �@�  @���@���@�l�@���@���@��R@���@�=q@�J@�@��#@��h@�p�@�`B@�/@��@��@��@�Z@�1@���@�|�@�;d@�@��!@�ff@��@��-@�7L@��u@�r�@��F@�|�@�33@�
=@��H@��+@�-@���@��@���@��@�z�@�j@�bN@�I�@���@�\)@�"�@�o@�
=@��@���@�V@��^@�?}@��9@���@�t�@�;d@��H@���@�n�@�=q@�J@��@���@�@���@�X@�Ĝ@�Q�@�1'@��@�b@�1@�  @���@��@��@���@��@���@�K�@���@�^5@���@�X@�/@�Ĝ@�bN@�1'@�b@�  @���@���@�S�@�@���@���@�v�@��@���@���@�x�@�X@�%@�ĜG�O�@�@�p�@�(�@�@uO�@j-@`�u@W�w@M�@F@>�@5�h@0  @)��@"�@I�@�+@��@/@
�!@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	VB	T�B	T�B	T�B	T�B	T�B	T�B	T�B	S�B	S�B	S�B	S�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	S�B	S�B	S�B	S�B	S�B	S�B	T�B	T�B	VB	XB	ZB	]/B	`BB	ffB	o�B	o�B	n�B	n�B	o�B	t�B	{�B	�B	�B	�B	�=B	��B	��B	��B
m�B
�B
�-B
��B
�ZB
�ZB
��B33BP�BVBQ�BG�BN�Br�B��B��B��B��B�B�FB�3B�B��B�+Bp�BT�BP�B>wBjBp�B<jB�B
�yB
�wB
��B
�PB
�B
w�B
cTB
E�B
(�B
�B
DB	��B	�B	�mB	�/B	ÖB	�B	�oB	s�B	VB	K�B	<jB	-B	!�B	�B	�B	�B	�B	�B	�B	PB	B��B	1B	1B	%B	%B	B	B	  B��B��B��B��B��B��B��B��B��B	B	PB	{B	'�B	49B	8RB	@�B	A�B	C�B	F�B	I�B	K�B	S�B	W
B	S�B	Q�B	M�B	K�B	M�B	P�B	VB	ZB	ZB	ZB	_;B	\)B	T�B	J�B	5?B	�B��B�B��B�}B�qBÖBɺB��B��B�5B�mB�NB�/B��BĜBBĜBɺB��B��B��B�B�;B�yB�B�B��B��B��B��B	B	1B	\B	bB	hB	�B	�B	$�B	(�B	)�B	+B	0!B	33B	49B	9XB	C�B	G�B	I�B	K�B	M�B	O�B	ZB	`BB	dZB	l�B	gmB	iyB	u�B	r�B	q�B	u�B	}�B	�1B	�=B	�=B	�7B	�uB	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�FB	�jB	�dB	�qB	��B	ÖB	ŢB	ƨB	ƨB	ǮB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�HB	�TB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
1B
+B
1B
	7B
+B
%B
%B
%B
%B
1B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
bB
bB
hB
oB
oB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
/B
8RB
8RB
?}B
E�B
M�B
S�B
XB
]/B
bNB
ffB
k�B
q�B
v�B
z�B
~�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	U�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	S�B	S�B	S�B	S�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	S�B	S�B	S�B	S�B	S�B	S�B	T�B	T�B	U�B	W�B	Y�B	]
B	`B	f>B	osB	ouB	noB	nnB	osB	t�B	{�B	��B	��B	��B	�B	�`B	��B	��B
mdB
��B
�B
йB
�-B
�0B
�B3BP�BU�BQ�BG�BN�Br�B�lB��B��B��B��B�B�B��B��B��BptBT�BP�B>FBjOBprB<8ByB
�GB
�HB
��B
�B
��B
w�B
c$B
EoB
(�B
OB
B	��B	�uB	�8B	��B	�^B	��B	�;B	sB	U�B	K�B	<1B	,�B	!�B	mB	XB	ZB	YB	XB	FB	B	�B��B	�B	�B	�B	�B	�B	�B��B��B��B��B��B��B��B��B��B��B	�B	B	?B	'�B	3�B	8B	@FB	AKB	CXB	FgB	I|B	K�B	S�B	V�B	S�B	Q�B	M�B	K�B	M�B	P�B	U�B	Y�B	Y�B	Y�B	^�B	[�B	T�B	J�B	4�B	TB��B�KBӹB�<B�0B�SB�xB˃BӶB��B�-B�B��B̈B�[B�MB�XB�vB͑BϛBӸB��B��B�6B�TB�kB��B��B��B��B	�B	�B	B	B	#B	HB	|B	$�B	(�B	)�B	*�B	/�B	2�B	3�B	9B	CSB	GjB	IwB	K�B	M�B	O�B	Y�B	_�B	dB	lFB	g*B	i7B	uB	rnB	qgB	u~B	}�B	��B	��B	��B	��B	�2B	�jB	�tB	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�#B	�-B	�EB	�RB	�_B	�eB	�dB	�kB	�cB	�kB	�kB	�oB	�qB	�xB	�}B	˃B	̈B	̋B	̈B	̋B	̌B	̊B	̈B	̆B	͏B	ϗB	ϛB	ϚB	͏B	̈B	˄B	�~B	�vB	�oB	˂B	̆B	̆B	͐B	ΕB	̆B	͏B	͍B	͍B	͌B	͎B	ΓB	ϚB	СB	ѦB	ПB	ТB	УB	ϙB	ОB	ѦB	ҰB	ӲB	վB	��B	�B	�B	�B	�(B	�1B	�KB	�YB	�[B	�XB	�^B	�_B	�WB	�YB	�PB	�`B	�_B	�]B	�`B	�^B	�jB	�jB	�iB	�oB	�qB	�oB	�qB	�wB	�~B	�B	�}B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B
B
B
 B
B
B
	B

B

B
	B
B

B
	B

B
	B
B
	B
B
B
B
B
B
B
"B
+B
(B
)B
)B
)B
*B
3B
=B
>B
?B
?B
?B
?B
BB
?B
?B
CB
>B
>B
;B
:B
1B
4B
5B
6B
-B
-B
5B
6B
6B
7B
4B
4B
7B
:B
:B
:B
;B
AB
AB
AB
BB
CB
@G�O�B
JB
eB
$�B
.�B
8
B
8B
?5B
E[B
M�B
S�B
W�B
\�B
bB
fB
k>B
qdB
v�B
z�B
~�B
��B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007572019040510075720190405100757  AO  ARCAADJP                                                                    20181121125949    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125949  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125949  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100757  IP                  G�O�G�O�G�O�                