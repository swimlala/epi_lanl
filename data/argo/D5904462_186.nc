CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:58Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125958  20190405100802  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�U��.�1   @�Vy\�B@-\�1&��c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @333@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C�fC"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D��D�6fD���D�ɚD�fD�<�D��3D���D���D�L�D�y�D��3D�fD�C3D�l�D��3D�	�D�<�D�p D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @U@�G�@�G�A��A(��AH��Ah��A�Q�A��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�G�B�{B�{B�{B�{B��HB��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�z�B��HB�{B�{B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C��C�=C p�C"�=C$p�C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN��CP��CR�=CTp�CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt�\Dy�\D�D�G�D���D���D��D�ND��{D��D��D�^D���D��{D�'�D�T{D�~D��{D��D�ND�HD�Ǯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A���A���A��A���A���A�  A���A��A��TA��AϼjA�dZA�(�A���A�A�~�A�jA�ffA�`BA�`BA�XA�S�A�M�A�K�A�I�A�C�A�A�A�9XA�/A� �A�{A��yAͧ�A�`BA�33A�%A��ȂhA�/A�-AȑhA��HAɕ�A��mA�hsA�ȴA�S�Aǣ�A���AŮA���A���Aß�A�;dA�(�A�(�A�G�A�p�A�  A�1A���A�;dA�A���A�A���A��A�1'A�n�A�&�A�K�A�  A���A�33A�r�A�bNA��9A�;dA��A��;A��A�E�A��!A�&�A�|�A��mA��+A��jA���A�5?A�dZA���A�ȴA� �A���A�{A�/A���A�l�A~�\A}S�A|-Ayl�Ap��Aj��Ab�RA\�`A[`BAZ^5AX�9AV�AUVAQ�;AO��AN�AM�hAJ�HAHr�AFVAEt�AE%AD��AB��AAp�A?&�A>�/A<��A:��A9�wA8��A8  A7XA4ffA2bA1�A1S�A1��A0��A0��A/�wA.n�A-XA,9XA+��A+O�A*bNA)�A(�A(I�A(5?A&�A&$�A%�A%�hA%K�A$��A$�A$~�A#�mA#��A#O�A"�A"-A AĜA�PA�A�A��A{A|�A�A�-A��A^5AI�AȴA~�AA|�A�A7LA�HA��AC�A�A/AĜA�;AdZA��A�A  A
=A	"�A �A��A�A��A�;A{A��A�A=qA5?A��A�AQ�A�AJA�hA@��;@��R@���@�n�@��R@�ȴ@���@��\@���@�V@�  @��@���@��\@�J@��@��7@�V@�Q�@�ƨ@���@�S�@�"�@���@���@�r�@��@�n�@��@�dZ@�l�@�+@�^@�|�@�@��/@�D@��;@�M�@���@�!@���@��@�M�@�-@�33@��@��@�E�@�@�|�@�{@�?}@�33@�V@�G�@��@�@�z�@�u@�9X@�S�@�M�@�Z@�{@�A�@��;@�bN@�b@�K�@�@�v�@��`@�j@�  @��@ו�@�|�@׶F@�  @ם�@ׅ@�l�@��@�E�@�x�@�ƨ@�=q@�hs@�p�@�~�@��@�@҇+@�5?@���@���@�X@Ͼw@�ȴ@�ff@�x�@˥�@ʟ�@ɲ-@ɲ-@�p�@ȃ@ǶF@���@���@� �@�j@ȣ�@�Ĝ@ȴ9@ȓu@�
=@�@�^5@�7L@��@þw@å�@Õ�@å�@��m@��@�z�@ě�@�Q�@öF@��@�@�5?@���@��@��@�j@�Z@�1@�|�@�C�@�"�@��@�ȴ@��R@���@�$�@��7@�%@�r�@��@�o@��\@��+@�~�@�M�@�{@���@�`B@�V@�bN@�  @��m@��F@��@�;d@�J@���@�X@�/@���@�j@�b@��;@�ƨ@���@�dZ@���@�n�@���@�O�@��@���@�S�@�t�@�l�@���@���@��7@�Ĝ@�A�@��
@�K�@�+@�"�@�
=@��@���@�n�@�J@�?}@��@�j@�ƨ@�+@��@��R@�{@���@�V@���@��`@���@���@�(�@��w@�l�@�33@���@�n�@�5?@�x�@���@��9@��D@�z�@�(�@���@�|�@��@��P@�K�@�;d@�
=@�E�@���@�G�@��/@��D@�bN@� �@�  @���@�S�@�o@���@��H@���@�E�@�E�@���@�`B@�%@���@��/@�Ĝ@���@�z�@�Q�@�(�@�1@���@��@��
@���@�\)@�"�@�ȴ@�v�@�{@���@��@�X@��@�ȴ@��
@��@}�@st�@j��@a�^@Xr�@Q&�@H �@?\)@9G�@1��@-�h@( �@!��@��@z�@��@�@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A���A���A��A���A���A�  A���A��A��TA��AϼjA�dZA�(�A���A�A�~�A�jA�ffA�`BA�`BA�XA�S�A�M�A�K�A�I�A�C�A�A�A�9XA�/A� �A�{A��yAͧ�A�`BA�33A�%A��ȂhA�/A�-AȑhA��HAɕ�A��mA�hsA�ȴA�S�Aǣ�A���AŮA���A���Aß�A�;dA�(�A�(�A�G�A�p�A�  A�1A���A�;dA�A���A�A���A��A�1'A�n�A�&�A�K�A�  A���A�33A�r�A�bNA��9A�;dA��A��;A��A�E�A��!A�&�A�|�A��mA��+A��jA���A�5?A�dZA���A�ȴA� �A���A�{A�/A���A�l�A~�\A}S�A|-Ayl�Ap��Aj��Ab�RA\�`A[`BAZ^5AX�9AV�AUVAQ�;AO��AN�AM�hAJ�HAHr�AFVAEt�AE%AD��AB��AAp�A?&�A>�/A<��A:��A9�wA8��A8  A7XA4ffA2bA1�A1S�A1��A0��A0��A/�wA.n�A-XA,9XA+��A+O�A*bNA)�A(�A(I�A(5?A&�A&$�A%�A%�hA%K�A$��A$�A$~�A#�mA#��A#O�A"�A"-A AĜA�PA�A�A��A{A|�A�A�-A��A^5AI�AȴA~�AA|�A�A7LA�HA��AC�A�A/AĜA�;AdZA��A�A  A
=A	"�A �A��A�A��A�;A{A��A�A=qA5?A��A�AQ�A�AJA�hA@��;@��R@���@�n�@��R@�ȴ@���@��\@���@�V@�  @��@���@��\@�J@��@��7@�V@�Q�@�ƨ@���@�S�@�"�@���@���@�r�@��@�n�@��@�dZ@�l�@�+@�^@�|�@�@��/@�D@��;@�M�@���@�!@���@��@�M�@�-@�33@��@��@�E�@�@�|�@�{@�?}@�33@�V@�G�@��@�@�z�@�u@�9X@�S�@�M�@�Z@�{@�A�@��;@�bN@�b@�K�@�@�v�@��`@�j@�  @��@ו�@�|�@׶F@�  @ם�@ׅ@�l�@��@�E�@�x�@�ƨ@�=q@�hs@�p�@�~�@��@�@҇+@�5?@���@���@�X@Ͼw@�ȴ@�ff@�x�@˥�@ʟ�@ɲ-@ɲ-@�p�@ȃ@ǶF@���@���@� �@�j@ȣ�@�Ĝ@ȴ9@ȓu@�
=@�@�^5@�7L@��@þw@å�@Õ�@å�@��m@��@�z�@ě�@�Q�@öF@��@�@�5?@���@��@��@�j@�Z@�1@�|�@�C�@�"�@��@�ȴ@��R@���@�$�@��7@�%@�r�@��@�o@��\@��+@�~�@�M�@�{@���@�`B@�V@�bN@�  @��m@��F@��@�;d@�J@���@�X@�/@���@�j@�b@��;@�ƨ@���@�dZ@���@�n�@���@�O�@��@���@�S�@�t�@�l�@���@���@��7@�Ĝ@�A�@��
@�K�@�+@�"�@�
=@��@���@�n�@�J@�?}@��@�j@�ƨ@�+@��@��R@�{@���@�V@���@��`@���@���@�(�@��w@�l�@�33@���@�n�@�5?@�x�@���@��9@��D@�z�@�(�@���@�|�@��@��P@�K�@�;d@�
=@�E�@���@�G�@��/@��D@�bN@� �@�  @���@�S�@�o@���@��H@���@�E�@�E�@���@�`B@�%@���@��/@�Ĝ@���@�z�@�Q�@�(�@�1@���@��@��
@���@�\)@�"�@�ȴ@�v�@�{@���@��@�X@��@�ȴ@��
@��@}�@st�@j��@a�^@Xr�@Q&�@H �@?\)@9G�@1��@-�h@( �@!��@��@z�@��@�@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�3B�3B�-B�-B�-B�9B�9B�-B�'B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�{B�{B�{B�{B�{B�{B��B��B��B��B��B��B��B��B�9B�LB�5B	p�B	�-B
6FB
y�B
��B
=BJBB
��B
�B
�BBK�B`BBs�B��B�BɺB�#B�sB�B�B�BB��BBoBhB\B	7B��B�B�)B�B�qB�B��B�+BbNBK�B49B �B+B
��B
�NB
�B
��B
��B
�oB
�B
�B
��B
�JB
y�B
m�B
cTB
W
B
H�B
=qB
�B	��B	�B	�mB	��B	��B	gmB	7LB	 �B	�B	�B	hB		7B	  B��B�B�B�mB�NB�)B�B�
B��B��B��B�B�5B�NB�HB�yB�B��B��B	B	bB	
=B	oB	�B	/B	8RB	D�B	F�B	F�B	J�B	XB	aHB	gmB	n�B	u�B	u�B	y�B	�B	�B	�B	�B	�+B	��B	��B	��B	��B	��B	�3B	�^B	�RB	�9B	��B	��B	��B	�\B	��B	�uB	�\B	�DB	�+B	�7B	�1B	�JB	�oB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�uB	�\B	��B	��B	�\B	�\B	��B	��B	�{B	�oB	�uB	�\B	�=B	�+B	�%B	�=B	�1B	�+B	�1B	�7B	�\B	�\B	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�!B	�3B	�FB	�?B	�3B	�XB	�dB	�RB	�FB	�FB	�dB	ȴB	ǮB	ǮB	ƨB	��B	B	��B	B	B	�wB	�}B	ɺB	��B	��B	��B	��B	��B	�B	�
B	�
B	�5B	�NB	�5B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�sB	�BB	�#B	�#B	�`B	�yB	�mB	�fB	�ZB	�HB	�BB	�;B	�/B	�NB	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�mB	�TB	�NB	�`B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�mB	�TB	�BB	�;B	�BB	�HB	�5B	�/B	�HB	�TB	�fB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�yB	�yB	�yB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
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
%B
B
B
B
B
B
B
1B
1B
+B
%B
%B
B
B
%B
%B
%B
1B
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

=B
JB
PB
PB
PB
PB
PB
PB
PB
\B
\B
\B
bB
\B
\B
\B
hB
hB
hB
hB
oB
uB
oB
{B
{B
�B
{B
{B
oB
hB
hB
oB
uB
uB
oB
uB
{B
{B
�B
�B
�B
{B
{B
{B
{B
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
.B
33B
9XB
A�B
E�B
J�B
P�B
VB
[#B
^5B
cTB
ffB
iyB
l�B
r�B
x�B
|�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�zB�dB�WB�VB�VB�]B�VB�PB�OB�PB�PB�RB�PB�PB�UB�nB��B��B��B��B��B��B�B�!B�B	pxB	� B
6B
y�B
��B
BB�B
��B
�hB
�dB�BK�B`Bs�B�QB��BɍB��B�DB�{B�cB�B�B��B�B;B6B*B	B��B�ZB��B��B�?B��B�~B��BbBK�B4B �B�B
��B
�B
��B
��B
��B
�;B
��B
��B
��B
�B
y�B
m^B
c B
V�B
H~B
=:B
�B	��B	�xB	�8B	̕B	�OB	g4B	7B	 �B	eB	MB	,B	�B��B��B�pB�VB�2B�B��B��B��B��BӼB��B��B��B�B�B�<B�sB��B��B	�B	$B		�B	1B	yB	.�B	8B	D]B	FjB	FjB	J�B	W�B	aB	g/B	n[B	u�B	u�B	y�B	��B	��B	��B	��B	��B	�AB	�gB	�hB	�tB	��B	��B	�!B	�B	��B	��B	�yB	�AB	�B	�BB	�5B	�B	�B	��B	��B	��B	�
B	�0B	��B	��B	��B	��B	��B	�eB	��B	��B	�lB	�dB	�jB	�wB	�yB	��B	��B	��B	��B	�tB	�3B	�B	�qB	�RB	�B	�B	�RB	�WB	�7B	�-B	�6B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�:B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�!B	�B	�B	�B	�B	�qB	�jB	�lB	�eB	�>B	�MB	�@B	�JB	�MB	�5B	�8B	�xB	͑B	ПB	ԹB	ӴB	˃B	��B	��B	��B	��B	�
B	��B	��B	ҰB	ХB	ΖB	��B	�;B	�uB	��B	�B	�qB	�\B	�0B	��B	��B	��B	�B	�6B	�*B	�#B	�B	�B	��B	��B	��B	�
B	�B	�&B	�UB	�MB	�UB	�aB	�[B	�aB	�HB	�(B	�B	�	B	�B	�YB	�dB	�yB	�wB	�xB	�|B	�zB	�B	�jB	�TB	�HB	�'B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�<B	�IB	�QB	�QB	�TB	�:B	�5B	�MB	�>B	�4B	�3B	�5B	�5B	�=B	�^B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
	B
	B
	B
B

B
B

B
B
B
B
B
B
B
B
"B
"B
#B
!B
*B
,B
)B
4B
4B
9B
3B
5B
*B
"B
"B
&B
-B
0B
'B
-B
5B
4B
<B
:B
;B
6B
7B
5B
6B
6B
7B
5B
8B
3B
6B
4B
5B
6B
>B
:B
;B
;B
=B
:B
;B
@B
AB
@B
DB
EB
HB
XB
yB
&�B
-�B
2�B
9B
A?B
E\B
JyB
P�B
U�B
Z�B
]�B
cB
f!B
i3B
lEB
rkB
x�B
|�B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008022019040510080220190405100802  AO  ARCAADJP                                                                    20181121125958    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125958  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125958  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100802  IP                  G�O�G�O�G�O�                