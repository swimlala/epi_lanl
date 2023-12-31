CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:50Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125950  20190405100757  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��l�bP�1   @��n>� F@0g-�d`�\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   AA��A`  A�  A�  A�33A�33A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D���D�@ D�y�D�� D�3D�FfD�p D�ٚD�3D�6fD���D�� D�� D�#3Dڃ3D���D��D�@ D�i�D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A(��AJ=qAh��A�Q�A�Q�A��A��A�Q�A�Q�A�Q�A�Q�B(�B
(�B�\B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�G�B�{B�G�B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�Q�C�Q�C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt�\Dy�\D�D�QHD���D��HD�${D�W�D��HD���D�${D�G�D���D��HD�HD�4{Dڔ{D��D�.D�QHD�z�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�|�A�|�A�|�A�~�AՁAՁAՃAՁAՃAՃAՃAՅAՅAՇ+AՉ7AՉ7AՋDAՋDAՋDAՍPAՍPAՏ\AՑhAՑhAՓuAՓuAՑhAՓuAՓuAՓuA՗�A՗�Aՙ�A՛�AՓuAՕ�AՕ�AՕ�A՗�Aՙ�A՛�Aՙ�Aՙ�A�?}A�?}A��Aԉ7A�VA���A�1'AƅA�/A��A�G�A�VA��A�hsA�oA���A���A�A�A�^5A��/A��!A�I�A��A���A��A��A��7A��mA�7LA�5?A��A�9XA��7A���A�`BA��\A�A�5?A�n�A���A���A�$�A���A�"�A�oA�E�A��A��yA��FA��A�n�A~-Ay�wAtQ�Ao;dAl��AlVAk�wAi�-Aa��A[�7AWAT  AO
=AMp�ALĜAJv�AF�AD5?A?S�A;|�A:r�A8�A5��A4��A37LA1�A05?A/hsA.E�A-��A,5?A*n�A(�A'A&��A%t�A$�jA#�-A#A!�;A��AE�A��A�HAl�A�DA9XAbA�\AdZA�A�
A7LA�DA�A��AE�A��A��AA��A��A�A
�uA
�A	��A	?}A(�A;dA��A��AK�A�AI�A1A&�A�RA�DAE�A�A�AhsA=qAAG�A;dA%A��A$�A�TA=qAA�A=qA1AƨA��A�AXA�AȴA�A�hA ��A ZA �@�33@���@�=q@���@�@��@��@�I�@���@�ȴ@�J@���@�V@���@���@�(�@�ƨ@�"�@���@�^5@��@�r�@�;d@�@�z�@�ȴ@�=q@@�=q@��@���@�O�@�D@�+@��y@�ȴ@�{@�^@�V@��
@�@�C�@�ȴ@�\@�M�@�-@�$�@�-@旍@�o@�!@�+@噚@�7L@��@��@�"�@��@���@�~�@�hs@�%@��
@�n�@�^5@�{@�@�X@��@�t�@��@ڧ�@��T@ى7@ٲ-@١�@ٙ�@�x�@�G�@��@��@��`@ؓu@��m@׍P@�|�@�t�@�C�@��y@�^5@Ձ@Ԭ@��@���@Լj@ԃ@��@�t�@�o@��H@҇+@��T@с@�?}@���@�Q�@�\)@Ο�@��@͑h@�%@�j@�b@�ƨ@ˍP@�+@ʰ!@�^5@ɺ^@��@�j@��
@��@�V@���@őh@�7L@ļj@�Q�@�\)@���@�^5@�@���@�hs@�V@�z�@�A�@�b@���@�"�@�o@�@��y@�v�@��@���@���@�bN@�b@�dZ@���@��\@�^5@��@���@�x�@�&�@���@�A�@�1@���@��P@���@���@�~�@�V@�5?@�$�@�$�@��#@�@�`B@�/@�%@��j@�bN@�b@��@��m@�ƨ@��y@���@���@�E�@�{@��T@��h@���@��@�1@�ƨ@�@�ff@�E�@�-@��^@�O�@�V@��9@�A�@���@��@�ȴ@�E�@���@���@�O�@���@�Z@�1'@�(�@���@��@��@�n�@�V@�J@�@�O�@���@�z�@�9X@�1@���@�33@��@�E�@�@��-@��@��
@�K�@���@��+@�ff@�=q@�J@���@��@���@���@�7L@��j@�b@��@��@��@���@�~�@�ff@�n�@�n�@�n�@��@��`@���@��u@��@�z�@�j@�I�@��m@��@�\)@�C�@�C�@�33@�@��y@���@���@�^5@�5?@�@��T@��^@�O�@���@���@�Q�@��@���@��
@���@�;d@�o@��y@�;d@���@��u@~ȴ@v$�@nE�@dI�@[��@R^5@G�w@?l�@7�;@2�H@,z�@( �@"�\@O�@1'@Z@bN@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�|�A�|�A�|�A�~�AՁAՁAՃAՁAՃAՃAՃAՅAՅAՇ+AՉ7AՉ7AՋDAՋDAՋDAՍPAՍPAՏ\AՑhAՑhAՓuAՓuAՑhAՓuAՓuAՓuA՗�A՗�Aՙ�A՛�AՓuAՕ�AՕ�AՕ�A՗�Aՙ�A՛�Aՙ�Aՙ�A�?}A�?}A��Aԉ7A�VA���A�1'AƅA�/A��A�G�A�VA��A�hsA�oA���A���A�A�A�^5A��/A��!A�I�A��A���A��A��A��7A��mA�7LA�5?A��A�9XA��7A���A�`BA��\A�A�5?A�n�A���A���A�$�A���A�"�A�oA�E�A��A��yA��FA��A�n�A~-Ay�wAtQ�Ao;dAl��AlVAk�wAi�-Aa��A[�7AWAT  AO
=AMp�ALĜAJv�AF�AD5?A?S�A;|�A:r�A8�A5��A4��A37LA1�A05?A/hsA.E�A-��A,5?A*n�A(�A'A&��A%t�A$�jA#�-A#A!�;A��AE�A��A�HAl�A�DA9XAbA�\AdZA�A�
A7LA�DA�A��AE�A��A��AA��A��A�A
�uA
�A	��A	?}A(�A;dA��A��AK�A�AI�A1A&�A�RA�DAE�A�A�AhsA=qAAG�A;dA%A��A$�A�TA=qAA�A=qA1AƨA��A�AXA�AȴA�A�hA ��A ZA �@�33@���@�=q@���@�@��@��@�I�@���@�ȴ@�J@���@�V@���@���@�(�@�ƨ@�"�@���@�^5@��@�r�@�;d@�@�z�@�ȴ@�=q@@�=q@��@���@�O�@�D@�+@��y@�ȴ@�{@�^@�V@��
@�@�C�@�ȴ@�\@�M�@�-@�$�@�-@旍@�o@�!@�+@噚@�7L@��@��@�"�@��@���@�~�@�hs@�%@��
@�n�@�^5@�{@�@�X@��@�t�@��@ڧ�@��T@ى7@ٲ-@١�@ٙ�@�x�@�G�@��@��@��`@ؓu@��m@׍P@�|�@�t�@�C�@��y@�^5@Ձ@Ԭ@��@���@Լj@ԃ@��@�t�@�o@��H@҇+@��T@с@�?}@���@�Q�@�\)@Ο�@��@͑h@�%@�j@�b@�ƨ@ˍP@�+@ʰ!@�^5@ɺ^@��@�j@��
@��@�V@���@őh@�7L@ļj@�Q�@�\)@���@�^5@�@���@�hs@�V@�z�@�A�@�b@���@�"�@�o@�@��y@�v�@��@���@���@�bN@�b@�dZ@���@��\@�^5@��@���@�x�@�&�@���@�A�@�1@���@��P@���@���@�~�@�V@�5?@�$�@�$�@��#@�@�`B@�/@�%@��j@�bN@�b@��@��m@�ƨ@��y@���@���@�E�@�{@��T@��h@���@��@�1@�ƨ@�@�ff@�E�@�-@��^@�O�@�V@��9@�A�@���@��@�ȴ@�E�@���@���@�O�@���@�Z@�1'@�(�@���@��@��@�n�@�V@�J@�@�O�@���@�z�@�9X@�1@���@�33@��@�E�@�@��-@��@��
@�K�@���@��+@�ff@�=q@�J@���@��@���@���@�7L@��j@�b@��@��@��@���@�~�@�ff@�n�@�n�@�n�@��@��`@���@��u@��@�z�@�j@�I�@��m@��@�\)@�C�@�C�@�33@�@��y@���@���@�^5@�5?@�@��T@��^@�O�@���@���@�Q�@��@���@��
@���@�;d@�o@��y@�;d@���@��u@~ȴ@v$�@nE�@dI�@[��@R^5@G�w@?l�@7�;@2�H@,z�@( �@"�\@O�@1'@Z@bN@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	B�B	A�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	A�B	B�B	B�B	A�B	A�B	A�B	B�B	A�B	A�B	B�B	A�B	A�B	A�B	B�B	A�B	A�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	A�B	C�B	C�B	B�B	C�B	B�B	B�B	B�B	B�B	B�B	Q�B	Q�B	XB	jB
�B
�FB
�)BB%B+B�B>wBC�BJ�BP�BXBffB� B�hB�bB��B�jB��B��B�+B�B��Bl�B>wB&�B
��B
�
B
�RB
�\B
o�B
>wB
#�B
�B
�B
JB
B
  B	�B	�fB	�;B	��B	�wB	�LB	�-B	��B	��B	��B	�%B	q�B	hsB	s�B	l�B	ffB	S�B	(�B	hB	B��B�B�yB�fB�fB�fB�sB�mB�fB�TB�BB�BB�HB�;B�/B�B�B�B�B�
B��B��B��B��B��B��B��B��B��B��B��BȴBɺBȴB��B��B��B�B��B��B��B��BȴBŢBÖB��B�}B�}B��B��B��B�}B��BŢB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�yB��B	VB	)�B	6FB	7LB	6FB	33B	2-B	2-B	@�B	N�B	W
B	YB	[#B	\)B	]/B	\)B	[#B	\)B	_;B	aHB	cTB	bNB	aHB	`BB	_;B	_;B	`BB	`BB	aHB	`BB	aHB	dZB	hsB	k�B	k�B	k�B	k�B	m�B	n�B	s�B	w�B	z�B	z�B	|�B	|�B	~�B	~�B	|�B	� B	�B	�DB	�JB	�uB	�uB	�{B	�uB	�oB	�oB	�oB	�uB	�uB	�uB	�oB	�{B	��B	��B	��B	��B	��B	�B	�B	�FB	�qB	�wB	�}B	B	ĜB	ĜB	ÖB	ÖB	ŢB	ƨB	ƨB	ƨB	ŢB	ŢB	ÖB	ŢB	ǮB	ɺB	��B	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�BB	�BB	�5B	�#B	�;B	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
VB
\B
\B
\B
\B
\B
\B
\B
VB
\B
VB
\B
\B
bB
bB
bB
bB
bB
bB
uB
{B
{B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
-B
6FB
:^B
;dB
A�B
G�B
M�B
S�B
YB
\)B
aHB
dZB
iyB
m�B
q�B
v�B
{�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	BfB	A`B	BdB	BdB	BcB	BdB	BcB	BcB	BdB	BeB	BcB	A`B	BbB	BfB	A^B	A^B	A^B	BeB	A`B	A`B	BeB	A]B	A[B	A_B	BcB	A]B	A_B	BhB	BeB	BeB	BhB	BfB	BeB	BcB	A_B	CkB	ClB	BfB	ClB	BeB	BeB	BdB	BfB	BcB	Q�B	Q�B	W�B	jUB
[B
�B
��B�B�B�BVB>LBCjBJ�BP�BW�Bf8B�B�<B�3B��B�>B�aB�RB��B��B��Bl]B>GB&�B
��B
��B
�"B
�,B
ojB
>FB
#�B
~B
RB
B
�B	��B	�yB	�5B	�B	͟B	�AB	�B	��B	��B	�rB	�VB	��B	qrB	h;B	s~B	lQB	f-B	S�B	(�B	0B	�B��B�VB�>B�,B�*B�+B�7B�/B�*B�B�B�B�
B��B��B��B��B��B��B��B��BҳBХBΚBΙBҴBШBΗBˉBˉBʁB�uB�yB�wBΜBԼB��B��BҳBϛB̌BʂB�vB�bB�XB�AB�;B�;B�?B�CB�DB�>B�FB�cB�}B�BˆB�B�}BʂB˅BУBФBϝB͐B˅B�B̋B��B�7B��B	B	)�B	6B	7B	6B	2�B	1�B	1�B	@AB	N�B	V�B	X�B	Z�B	[�B	\�B	[�B	Z�B	[�B	^�B	aB	cB	bB	aB	_�B	^�B	^�B	_�B	_�B	aB	`B	aB	dB	h0B	kCB	kAB	kCB	kDB	mNB	nWB	srB	w�B	z�B	z�B	|�B	|�B	~�B	~�B	|�B	�B	��B	�B	�B	�/B	�/B	�7B	�2B	�,B	�+B	�,B	�1B	�2B	�5B	�,B	�7B	�QB	�tB	��B	��B	��B	��B	��B	�B	�.B	�4B	�8B	�KB	�[B	�YB	�RB	�TB	�^B	�cB	�fB	�eB	�`B	�[B	�SB	�_B	�iB	�xB	�|B	�~B	�wB	�pB	�oB	̊B	ϙB	үB	ԻB	տB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�)B	�)B	�*B	�-B	�5B	�@B	�@B	�@B	�AB	�BB	�AB	�AB	�>B	�@B	�EB	�EB	�GB	�BB	�@B	�FB	�DB	�KB	�EB	�HB	�FB	�DB	�GB	�DB	�?B	�AB	�@B	�;B	�>B	�@B	�8B	�:B	�:B	�;B	�@B	�?B	�AB	�@B	�KB	�TB	�TB	�UB	�QB	�NB	�OB	�LB	�GB	�GB	�EB	�JB	�EB	�LB	�KB	�LB	�MB	�JB	�LB	�MB	�RB	�WB	�RB	�XB	�YB	�[B	�lB	�lB	�lB	�nB	�lB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
/B
6B
6B
<B
;B
3B
5B
?B
?B
FB
HB
FB
IB
GB
HB
JB
GB
FB
KB
LB
DB
LB
NB
NB
MB
UB
TB
LB
SB
TB
SB
`B
lB
qB
#�B
,�B
6B
:B
;B
ACB
GhB
M�B
S�B
X�B
[�B
a B
dB
i3B
mKB
qeB
v�B
{�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007572019040510075720190405100757  AO  ARCAADJP                                                                    20181121125950    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125950  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125950  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100757  IP                  G�O�G�O�G�O�                