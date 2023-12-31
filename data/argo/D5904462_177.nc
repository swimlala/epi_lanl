CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:56Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125956  20190405100800  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��y���c1   @��zWW�@/�KƧ��c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dty�Dys3D� D�C3D���D�� D��D�9�D�vfD���D�fD�C3D�y�D��3D�  D�33D�l�D�ٚD�  D�C3D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A��A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:p�C<p�C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN��CP��CR��CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D)D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'�)D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-�)D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@)D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr�)Ds"�Ds��Dt"�Dt�)Dy��D�!HD�T{D���D��HD�*�D�J�D���D��D�'�D�T{D���D��{D�HD�D{D�~D���D�HD�T{D�~D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A��TAɼjAɬAɣ�Aɝ�Aɗ�Aɕ�Aɏ\Aɉ7AɁA�r�A�bNA�E�A��A�
=A�A���A���A��A��A��A��A��A���A�bA��A�{A�VA�JA�VA�VA�  AȮA��AƾwAŗ�A�l�A�+A��AĸRA�O�A�
=Aú^AÝ�A�bA�ƨA�`BA¬A�|�A�ZAA�ĜA�?}A�ȴA���A�^5A���A��-A� �A��9A�x�A��-A�-A�bA� �A��HA���A���A���A�
=A�VA�Q�A�=qA�/A�v�A�Q�A��\A�C�A�\)A��^A��A�`BA�9XA���A�dZA�z�A��A���A��TA�ĜA���A�ȴA�C�A��A��`A�
=A�  A��HAzE�Ax��Av��Au�
AuVAr(�Am��Ag��A`M�A]�PAZ��AU33AS|�APbNAL�`AI�7AGl�AE��ACƨAB-AA�A> �A:�yA:Q�A9��A8��A7|�A3A1l�A.�RA,�A+�mA)�
A'l�A$�HA#��A"ZA!"�A��AQ�A�DA��AS�A �\A!K�A"��A$v�A$�/A$�HA%"�A$��A%�A'A&�A&ȴA&��A%A#�#A#?}A"�`A"�uA"-A!�-A �+A =qA �A��A�;A��A��A�+A �A��AC�AȴAr�A��A\)A;dA�A��A��A��A�
A�-AG�A
=A~�AbNA1A/A�A�A��A�A��AZA-AĜA�\A�#AA��A9XAoA��A��A+AȴA^5A�A�wA�yA��AS�A�#A�RA33Az�AƨA�A�!A�;@�A�@���@���@�^5@�M�@�x�@�%@�bN@�b@��@�1'@�j@�Q�@���@��;@�C�@���@�V@��-@�l�@�{@�p�@�@�A�@�b@���@�@���@��@��H@��@�+@��@�n�@�!@�!@�=q@�J@���@�/@�+@�@�G�@�j@䛦@���@� �@��;@�1@�b@��;@�5?@���@�Ĝ@��u@�j@�A�@�  @���@߾w@ߕ�@�@�V@��T@��@���@ݡ�@�hs@�X@�Ĝ@��@ۍP@�@�=q@�Ĝ@�j@׍P@�o@ָR@�-@թ�@ԃ@ӕ�@ӕ�@�l�@�^5@�p�@��`@���@�bN@��@ϥ�@�|�@�ȴ@�M�@�@�G�@̓u@�Q�@�b@���@˝�@��@ʰ!@�M�@�@��#@ɡ�@�&�@��@��@�%@�z�@�Q�@�bN@��@Ǖ�@�K�@�@�/@Ų-@Ł@��@���@ēu@î@Õ�@�C�@�@�J@�/@�j@� �@��@�o@���@�5?@��#@�p�@��@��@���@���@�C�@�"�@��@���@�X@��@���@�Q�@� �@�b@��;@�+@�E�@�M�@�-@�@��#@��-@���@�G�@��@�l�@��H@��R@�{@�x�@�X@��@��`@���@��
@�C�@���@�
=@�;d@�t�@�S�@�+@��@��!@�J@�p�@�p�@�hs@�`B@�X@�G�@��@�I�@��;@���@�;d@�o@��!@��+@�M�@�@�G�@���@�bN@� �@���@�+@��@��y@��H@��@�
=@���@��H@���@�$�@�O�@��`@�Ĝ@���@�bN@��F@�;d@���@��!@�{@�p�@���@���@�ƨ@�\)@�+@��@�ff@�5?@���@�`B@���@��D@��@��P@�|�@�C�@�
=@��y@��@�
=@�ȴ@���@�-@�p�@��`@���@�bN@�1'@��
@��F@��P@�\)@�;d@�o@��y@��!@�M�@���@�`B@��@�
=@��7@��D@��@yX@nff@fff@` �@W�@OK�@H�`@AX@:�@4��@/;d@(�9@$��@
=@x�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�
=A��TAɼjAɬAɣ�Aɝ�Aɗ�Aɕ�Aɏ\Aɉ7AɁA�r�A�bNA�E�A��A�
=A�A���A���A��A��A��A��A��A���A�bA��A�{A�VA�JA�VA�VA�  AȮA��AƾwAŗ�A�l�A�+A��AĸRA�O�A�
=Aú^AÝ�A�bA�ƨA�`BA¬A�|�A�ZAA�ĜA�?}A�ȴA���A�^5A���A��-A� �A��9A�x�A��-A�-A�bA� �A��HA���A���A���A�
=A�VA�Q�A�=qA�/A�v�A�Q�A��\A�C�A�\)A��^A��A�`BA�9XA���A�dZA�z�A��A���A��TA�ĜA���A�ȴA�C�A��A��`A�
=A�  A��HAzE�Ax��Av��Au�
AuVAr(�Am��Ag��A`M�A]�PAZ��AU33AS|�APbNAL�`AI�7AGl�AE��ACƨAB-AA�A> �A:�yA:Q�A9��A8��A7|�A3A1l�A.�RA,�A+�mA)�
A'l�A$�HA#��A"ZA!"�A��AQ�A�DA��AS�A �\A!K�A"��A$v�A$�/A$�HA%"�A$��A%�A'A&�A&ȴA&��A%A#�#A#?}A"�`A"�uA"-A!�-A �+A =qA �A��A�;A��A��A�+A �A��AC�AȴAr�A��A\)A;dA�A��A��A��A�
A�-AG�A
=A~�AbNA1A/A�A�A��A�A��AZA-AĜA�\A�#AA��A9XAoA��A��A+AȴA^5A�A�wA�yA��AS�A�#A�RA33Az�AƨA�A�!A�;@�A�@���@���@�^5@�M�@�x�@�%@�bN@�b@��@�1'@�j@�Q�@���@��;@�C�@���@�V@��-@�l�@�{@�p�@�@�A�@�b@���@�@���@��@��H@��@�+@��@�n�@�!@�!@�=q@�J@���@�/@�+@�@�G�@�j@䛦@���@� �@��;@�1@�b@��;@�5?@���@�Ĝ@��u@�j@�A�@�  @���@߾w@ߕ�@�@�V@��T@��@���@ݡ�@�hs@�X@�Ĝ@��@ۍP@�@�=q@�Ĝ@�j@׍P@�o@ָR@�-@թ�@ԃ@ӕ�@ӕ�@�l�@�^5@�p�@��`@���@�bN@��@ϥ�@�|�@�ȴ@�M�@�@�G�@̓u@�Q�@�b@���@˝�@��@ʰ!@�M�@�@��#@ɡ�@�&�@��@��@�%@�z�@�Q�@�bN@��@Ǖ�@�K�@�@�/@Ų-@Ł@��@���@ēu@î@Õ�@�C�@�@�J@�/@�j@� �@��@�o@���@�5?@��#@�p�@��@��@���@���@�C�@�"�@��@���@�X@��@���@�Q�@� �@�b@��;@�+@�E�@�M�@�-@�@��#@��-@���@�G�@��@�l�@��H@��R@�{@�x�@�X@��@��`@���@��
@�C�@���@�
=@�;d@�t�@�S�@�+@��@��!@�J@�p�@�p�@�hs@�`B@�X@�G�@��@�I�@��;@���@�;d@�o@��!@��+@�M�@�@�G�@���@�bN@� �@���@�+@��@��y@��H@��@�
=@���@��H@���@�$�@�O�@��`@�Ĝ@���@�bN@��F@�;d@���@��!@�{@�p�@���@���@�ƨ@�\)@�+@��@�ff@�5?@���@�`B@���@��D@��@��P@�|�@�C�@�
=@��y@��@�
=@�ȴ@���@�-@�p�@��`@���@�bN@�1'@��
@��F@��P@�\)@�;d@�o@��y@��!@�M�@���@�`B@��@�
=@��7@��D@��@yX@nff@fff@` �@W�@OK�@H�`@AX@:�@4��@/;d@(�9@$��@
=@x�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B��B��B��B��B��B��B��B	B	PB	�B	�B	 �B	 �B	!�B	"�B	"�B	"�B	"�B	"�B	+B	:^B	F�B	e`B	�B	��B	�qB	��B	�B
@�B
��B2-BaHBy�B}�Bx�Bt�Bm�Bl�B�B�hBB��B��B��B�yB��B%�B<jB6FB(�B�B�B�B%�B)�B+B.B9XBD�BG�BP�BS�BR�BS�BM�BF�BA�B6FB$�B�B�BB��B��B�B�B�HB�
BȴB��B�B�+B\)B@�B%�BoB
��B
��B
�bB
ffB
@�B
,B
uB	�`B	�B	��B	�DB	�B	{�B	e`B	F�B	�B��B�B�HB�
B�
B��B��B�
B�5B�BB�5B�#B�
B�5B�B�
B�B�B�sB�`B�B�mB�fB�ZB�BǮB�XB�-B�B��B��B��B��B�dB��B�yB��B	#�B	VB	o�B	�B	��B	��B	��B	��B
B
+B
%B	��B	��B	��B	��B
  B
B
+B
JB
oB
�B
�B
�B
#�B
$�B
"�B
!�B
"�B
"�B
$�B
#�B
&�B
)�B
)�B
)�B
,B
0!B
1'B
33B
2-B
0!B
/B
/B
1'B
1'B
,B
&�B
 �B
�B
bB
JB
VB
uB
 �B
"�B
�B
�B
bB	��B	�B	�ZB	�yB	�HB	�FB	��B	��B	�XB	��B	��B	ǮB	��B	�NB	�yB	�mB	��B	�B	�;B	��B	��B	�B	�9B	�PB	{�B	{�B	~�B	�B	�B	�1B	�VB	��B	��B	��B	��B	��B	�B	�B	�?B	��B	�qB	�^B	�XB	�qB	B	B	B	ĜB	ǮB	��B	��B	��B	�
B	�B	�)B	�/B	�5B	�BB	�BB	�5B	�B	��B	��B	��B	�B	�B	�B	�B	�/B	�;B	�5B	�B	�
B	�B	�B	�#B	�#B	�B	�B	�B	�B	�)B	�)B	�5B	�NB	�ZB	�ZB	�ZB	�fB	�mB	�mB	�mB	�`B	�TB	�NB	�BB	�BB	�BB	�BB	�BB	�HB	�BB	�;B	�HB	�NB	�NB	�HB	�;B	�5B	�5B	�5B	�5B	�;B	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�TB	�TB	�`B	�yB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB

=B
	7B
1B
+B
1B
1B
+B
+B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
	7B

=B
DB
DB
DB
JB
DB
DB

=B
DB
DB
JB
DB
DB
JB
DB
PB
PB
PB
VB
\B
\B
\B
\B
\B
bB
bB
\B
\B
\B
\B
\B
hB
bB
hB
oB
oB
uB
{B
{B
uB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
,B
2-B
5?B
=qB
F�B
J�B
O�B
R�B
W
B
ZB
_;B
dZB
gmB
l�B
o�B
s�B
v�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B��B��B�B��B��B��B��B	�B	%B	{B	�B	 �B	 �B	!�B	"�B	"�B	"�B	"�B	"�B	*�B	:1B	F{B	e5B	��B	��B	�EB	��B	�aB
@UB
ѿB2BaBy�B}�Bx�Bt�BmaBl^B��B�<B�bBϳBйB��B�IB��B%�B<:B6B(�BxBfB�B%�B)�B*�B-�B9&BDmBGBP�BS�BR�BS�BM�BFsBA[B6B$�BkBOB�B��B��B�wB�PB�B��BȂB�UB��B��B[�B@OB%�B=B
��B
ѶB
�+B
f0B
@PB
+�B
=B	�'B	��B	�uB	�B	��B	{�B	e(B	FqB	�B��B�YB�B��B��B��BϥB��B��B�B��B��B��B��B��B��B��B��B�7B�$B�WB�/B�)B�B��B�pB�B��B��B��B�gB�fB��B�%BʃB�<B��B	#�B	U�B	o]B	��B	�bB	��B	ΙB	��B
�B
�B
�B	��B	�|B	�~B	��B	��B
�B
�B
B
1B
FB
[B
B
#�B
$�B
"�B
!�B
"�B
"�B
$�B
#�B
&�B
)�B
)�B
)�B
+�B
/�B
0�B
2�B
1�B
/�B
.�B
.�B
0�B
0�B
+�B
&�B
 �B
?B
!B
B
B
2B
 �B
"�B
vB
lB
!B	��B	�HB	�B	�8B	�B	�B	�{B	��B	�B	̋B	�B	�lB	ѩB	�B	�8B	�-B	��B	�\B	��B	�GB	��B	��B	��B	�
B	{�B	{�B	~�B	��B	��B	��B	�B	�OB	�kB	�tB	��B	��B	��B	��B	��B	�GB	�.B	�B	�B	�,B	�IB	�KB	�MB	�XB	�lB	˂B	ΙB	ӴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ԹB	ҮB	ҮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�,B	�)B	�)B	�B	�B	�	B	��B	��B	��B	��B	� B	�B	��B	��B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�4B	�HB	�fB	�eB	�ZB	�]B	�~B	�}B	�uB	��B	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
 B

�B
B

�B

�B
	�B
B

�B
B
 B

�B
B

�B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
B
!B
&B
)B
/B
5B
5B
-B
*B
&B
+B
-B
5B
5B
5B
6B
:B
@B
@B
?B
HB
IB
GB
OB
MB
^B
&�B
+�B
1�B
4�B
=)B
FaB
JzB
O�B
R�B
V�B
Y�B
^�B
dB
g&B
lFB
oYB
srB
v�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008002019040510080020190405100800  AO  ARCAADJP                                                                    20181121125956    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125956  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125956  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100800  IP                  G�O�G�O�G�O�                