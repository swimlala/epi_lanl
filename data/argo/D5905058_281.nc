CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-12T21:39:43Z creation;2020-12-12T21:39:45Z conversion to V3.1;2023-06-29T05:47:44Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ip   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ML   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ̌   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201212213943  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0675_281                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Ng�d� 1   @�Nh�/h�@6gE8�4��b��c�	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B

=B
=B
=B"
=B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bb
=Bj
=Br
=Bz
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD��D�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD��D�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�D�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD���D��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRDRD��RD�RD�PRDÐRD��RD�RD�PRDĐRD��RD�RD�PRDŐRD��RD�RD�PRDƐRD��RD�RD�PRDǐRD��RD�RD�PRDȐRD��RD�RD�PRDɐRD��RD�RD�PRDʐRD��RD�RD�PRDːRD��RD�RD�PRD̐RD��RD�RD�PRD͐RD��RD�RD�PRDΐRD��RD�RD�PRDϐRD��RD�RD�PRDАRD��RD�RD�PRDѐRD��RD�RD�PRDҐRD��RD�RD�PRDӐRD��RD�RD�PRDԐRD��RD�RD�PRDՐRD��RD�RD�PRD֐RD��RD�RD�PRDאRD��RD�RD�PRDؐRD��RD�RD�PRDِRD��RD�RD�PRDڐRD��RD�RD�PRDېRD��RD�RD�PRDܐRD��RD�RD�PRDݐRD��RD�RD�PRDސRD��RD�RD�PRDߐRD��RD�RD�PRD��RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�MD�RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD�ӅD�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aȗ�Aȟ�Aȟ�Aȟ�Aȡ�Aȡ�Aȟ�Aȟ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȥ�Aȣ�Aș�AȍPA�XA�|�A��;Aƥ�A�=qA�bNAĶFA��
A���A�ȴA��yA��+A�x�A��DA� �A��-A��A���A�r�A��A�\)A��A�O�A�oA�ƨA��A�^5A��A�
=A��9A���A��/A�A��A�
=A�hsA�jA��jA���A��A�&�A��yA��^A�v�A��A�hsA�E�A�bA��A�p�A���A�|�A��wA��yA��A�K�A��A��/A�XA��yA�hsA�\)A�x�A�JA��9A��uA��A�{A�O�A�  A�dZA��
A��A��A� �A�|�A��
Ax�A}�A|E�A{/Ax�RAu�ArM�Ao�Aj��Ag|�Af��Af9XAe+Ac�hAbA�A`��A_��A[�AX�uAU��AU%AS�AQ�^APE�AN�AM��ALE�AJ�/AIhsAG��AF�AEdZAC�
ABjAA\)A?�A=�A<��A:ffA8{A6~�A4�+A3/A2�9A1�PA0�A0(�A/��A.n�A-A,�A,E�A,1'A,$�A,JA+;dA)�^A(�jA(VA(�A'&�A&=qA%�TA$ffA#hsA"-A!\)A!
=A �/A �!A $�A`BA�jA��AȴA�-A�A�A�\AbNA1'A7LA{At�A��A��A��A^5A�A��A{A�A�A^5A��A	�TA��A~�A�mA�!A|�A"�A��A�^A�\A �A��A ��A bNA @��!@��@�
=@�@���@�;d@�hs@��H@��@� �@�v�@�hs@�t�@�&�@��@�I�@�+@�V@�I�@��m@޸R@ݩ�@�@ٲ-@�1'@��@Ԭ@���@�M�@Ѻ^@�j@ύP@���@Ο�@͙�@�z�@�Q�@�ƨ@�@�-@ə�@��@��;@�"�@Ƨ�@�ff@���@ģ�@��@�+@�V@��@���@�?}@���@�Ĝ@��9@�r�@�Q�@� �@���@��H@�~�@�~�@�n�@���@�x�@�O�@�7L@�%@���@�j@��@��H@�ff@�=q@��-@�x�@��u@���@��#@��@�Ĝ@�(�@�\)@��y@���@�G�@�&�@�9X@�n�@�J@�G�@���@��@�(�@�1'@���@�\)@�n�@��@���@�p�@�?}@�/@�bN@���@��R@�=q@�@��7@�G�@�V@���@�Q�@��@���@�;d@�5?@��@�X@�%@��`@��`@��u@�  @��
@���@��F@�o@�v�@��@��#@���@�V@��9@�z�@��D@��@�A�@���@�t�@�\)@�C�@�33@��@���@��R@���@���@�M�@���@��T@��-@�x�@�hs@�X@�X@�O�@�?}@���@���@�z�@�9X@�b@�  @���@��@��;@�t�@��@�
=@�ȴ@��\@�v�@�5?@��@���@��-@�x�@�?}@��`@��@��@�Z@��@�  @��F@�l�@�;d@�
=@��R@���@��+@�ff@�5?@�@���@��h@�hs@�/@���@�Ĝ@�Ĝ@��j@���@�z�@�(�@��@�b@��@�ƨ@��F@��@���@�\)@�K�@�K�@�33@�@���@�ff@�E�@�$�@�{@��T@��^@���@��7@��7@�`B@���@���@�Ĝ@���@�Q�@�1'@�(�@� �@�  @���@��@��
@��
@���@�l�@�;d@��@���@�5?@���@��#@�@��-@��@�V@��D@�A�@�  @��
@��w@��@�dZ@�K�@�33@�@���@�M�@�@���@���@��@�`B@��@��`@���@��9@���@��u@�9X@�  @��m@��
@��P@�K�@�"�@��H@��R@���@��+@�M�@��@���@��-@���@�x�@�G�@���@�9X@�  @;d@
=@~�R@~5?@}�T@}�h@}O�@}V@|��@|��@{t�@z�\@y�@yhs@y%@xĜ@x �@w��@w|�@w\)@v�@u@u`B@u`B@u?}@tZ@sƨ@st�@s33@s@r�H@rM�@rJ@q��@qX@q�@q%@p��@p�u@pA�@p1'@p �@o��@oK�@n�y@nv�@n@m��@m?}@l��@l�@kdZ@k"�@ko@k@j�!@jn�@i��@h�9@hQ�@g�@g|�@gl�@g+@fȴ@f��@f��@f��@f�+@fV@fE�@e�h@e?}@eV@dj@dj@dz�@dz�@dj@dj@dj@dI�@c�m@c�F@cS�@b��@b~�@b=q@a�@ahs@a&�@`��@`�@`bN@`Q�@`1'@`  @_�@_�;@_l�@^��@^�@^��@^v�@]�T@]p�@]?}@]V@\��@\Z@\9X@\�@[�m@[dZ@Z�!@Y�^@Y&�@X��@XA�@W�;@WK�@V�y@V�R@V�+@VE�@V@U��@Up�@U?}@T�@T�j@Tz�@T(�@T1@S��@SS�@So@R��@R�\@R~�@R^5@R=q@R�@Q��@Q��@Q�7@Q7L@P��@P�u@PQ�@P  @O|�@N�y@N�R@M�@M��@Mp�@MO�@MV@L�/@L�@Lz�@Lj@L9X@K�m@K��@KdZ@K"�@J�@J�!@J~�@Jn�@J^5@J�@I�#@I��@I��@Ihs@IG�@I&�@I%@H�@Hb@G�@Gl�@GK�@G
=@F�@F�@F�R@F5?@F{@E�T@E��@E@E�h@E`B@E�@D��@DZ@D(�@C��@Cƨ@C��@C��@C"�@B�@B��@B��@B�!@B�\@BM�@B=q@B�@A�#@A�7@AG�@A&�@A%@A%@@��@@r�@@1'@?��@?��@?�P@?|�@?+@>��@>$�@>$�@=�@=`B@<�j@<�@<��@<z�@<�@;ƨ@;��@;�@;S�@;@:~�@:M�@9��@9��@9x�@97L@8�9@81'@7�w@7��@7|�@7K�@7�@6�y@6ȴ@6��@65?@5��@5`B@4��@4�j@4�D@4I�@49X@4�@3�F@333@3o@3@2��@2��@2n�@2=q@2J@1��@1��@1�7@17L@17L@1�@0�`@0Ĝ@0��@0�@0bN@0A�@0  @/|�@/;d@/�@.��@.�R@.ff@.5?@.@-�@-��@-@-�h@-`B@-?}@,�@,�@,I�@+�m@+t�@+C�@+@*��@*�\@*M�@)�#@)%@(��@(�@(bN@'�;@'��@'�@'\)@';d@'
=@&�y@&ȴ@&�+@&E�@&5?@&$�@%�T@%�T@%@%�-@%�@%�@%p�@%`B@%O�@%?}@%/@%/@%�@$�@$�D@#�
@#dZ@"�@"�!@!��@!%@ �u@ b@�w@�w@��@\)@+@�@�y@��@�+@v�@V@$�@�@�T@��@�@�@�@�@p�@O�@�@��@�@�j@j@ƨ@��@��@��@dZ@C�@33@o@�@��@��@��@n�@�@��@x�@7L@��@Ĝ@Ĝ@�9@�@bN@b@b@  @�w@�@l�@�@��@��@v�@5?@$�@@�T@�-@�@`B@?}@V@��@�@�@�/@��@�j@z�@Z@I�@9X@(�@1@��@��@�m@ƨ@�@dZ@33@33@o@��@��@�!@�!@��@��@~�@~�@n�@^5@=q@-@��@��@�^@�7@x�@�7@��@hs@7L@7L@�@��@bN@ �@  @�w@K�@��@��@v�@ff@5?@��@�-@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aȗ�Aȟ�Aȟ�Aȟ�Aȡ�Aȡ�Aȟ�Aȟ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȡ�Aȥ�Aȣ�Aș�AȍPA�XA�|�A��;Aƥ�A�=qA�bNAĶFA��
A���A�ȴA��yA��+A�x�A��DA� �A��-A��A���A�r�A��A�\)A��A�O�A�oA�ƨA��A�^5A��A�
=A��9A���A��/A�A��A�
=A�hsA�jA��jA���A��A�&�A��yA��^A�v�A��A�hsA�E�A�bA��A�p�A���A�|�A��wA��yA��A�K�A��A��/A�XA��yA�hsA�\)A�x�A�JA��9A��uA��A�{A�O�A�  A�dZA��
A��A��A� �A�|�A��
Ax�A}�A|E�A{/Ax�RAu�ArM�Ao�Aj��Ag|�Af��Af9XAe+Ac�hAbA�A`��A_��A[�AX�uAU��AU%AS�AQ�^APE�AN�AM��ALE�AJ�/AIhsAG��AF�AEdZAC�
ABjAA\)A?�A=�A<��A:ffA8{A6~�A4�+A3/A2�9A1�PA0�A0(�A/��A.n�A-A,�A,E�A,1'A,$�A,JA+;dA)�^A(�jA(VA(�A'&�A&=qA%�TA$ffA#hsA"-A!\)A!
=A �/A �!A $�A`BA�jA��AȴA�-A�A�A�\AbNA1'A7LA{At�A��A��A��A^5A�A��A{A�A�A^5A��A	�TA��A~�A�mA�!A|�A"�A��A�^A�\A �A��A ��A bNA @��!@��@�
=@�@���@�;d@�hs@��H@��@� �@�v�@�hs@�t�@�&�@��@�I�@�+@�V@�I�@��m@޸R@ݩ�@�@ٲ-@�1'@��@Ԭ@���@�M�@Ѻ^@�j@ύP@���@Ο�@͙�@�z�@�Q�@�ƨ@�@�-@ə�@��@��;@�"�@Ƨ�@�ff@���@ģ�@��@�+@�V@��@���@�?}@���@�Ĝ@��9@�r�@�Q�@� �@���@��H@�~�@�~�@�n�@���@�x�@�O�@�7L@�%@���@�j@��@��H@�ff@�=q@��-@�x�@��u@���@��#@��@�Ĝ@�(�@�\)@��y@���@�G�@�&�@�9X@�n�@�J@�G�@���@��@�(�@�1'@���@�\)@�n�@��@���@�p�@�?}@�/@�bN@���@��R@�=q@�@��7@�G�@�V@���@�Q�@��@���@�;d@�5?@��@�X@�%@��`@��`@��u@�  @��
@���@��F@�o@�v�@��@��#@���@�V@��9@�z�@��D@��@�A�@���@�t�@�\)@�C�@�33@��@���@��R@���@���@�M�@���@��T@��-@�x�@�hs@�X@�X@�O�@�?}@���@���@�z�@�9X@�b@�  @���@��@��;@�t�@��@�
=@�ȴ@��\@�v�@�5?@��@���@��-@�x�@�?}@��`@��@��@�Z@��@�  @��F@�l�@�;d@�
=@��R@���@��+@�ff@�5?@�@���@��h@�hs@�/@���@�Ĝ@�Ĝ@��j@���@�z�@�(�@��@�b@��@�ƨ@��F@��@���@�\)@�K�@�K�@�33@�@���@�ff@�E�@�$�@�{@��T@��^@���@��7@��7@�`B@���@���@�Ĝ@���@�Q�@�1'@�(�@� �@�  @���@��@��
@��
@���@�l�@�;d@��@���@�5?@���@��#@�@��-@��@�V@��D@�A�@�  @��
@��w@��@�dZ@�K�@�33@�@���@�M�@�@���@���@��@�`B@��@��`@���@��9@���@��u@�9X@�  @��m@��
@��P@�K�@�"�@��H@��R@���@��+@�M�@��@���@��-@���@�x�@�G�@���@�9X@�  @;d@
=@~�R@~5?@}�T@}�h@}O�@}V@|��@|��@{t�@z�\@y�@yhs@y%@xĜ@x �@w��@w|�@w\)@v�@u@u`B@u`B@u?}@tZ@sƨ@st�@s33@s@r�H@rM�@rJ@q��@qX@q�@q%@p��@p�u@pA�@p1'@p �@o��@oK�@n�y@nv�@n@m��@m?}@l��@l�@kdZ@k"�@ko@k@j�!@jn�@i��@h�9@hQ�@g�@g|�@gl�@g+@fȴ@f��@f��@f��@f�+@fV@fE�@e�h@e?}@eV@dj@dj@dz�@dz�@dj@dj@dj@dI�@c�m@c�F@cS�@b��@b~�@b=q@a�@ahs@a&�@`��@`�@`bN@`Q�@`1'@`  @_�@_�;@_l�@^��@^�@^��@^v�@]�T@]p�@]?}@]V@\��@\Z@\9X@\�@[�m@[dZ@Z�!@Y�^@Y&�@X��@XA�@W�;@WK�@V�y@V�R@V�+@VE�@V@U��@Up�@U?}@T�@T�j@Tz�@T(�@T1@S��@SS�@So@R��@R�\@R~�@R^5@R=q@R�@Q��@Q��@Q�7@Q7L@P��@P�u@PQ�@P  @O|�@N�y@N�R@M�@M��@Mp�@MO�@MV@L�/@L�@Lz�@Lj@L9X@K�m@K��@KdZ@K"�@J�@J�!@J~�@Jn�@J^5@J�@I�#@I��@I��@Ihs@IG�@I&�@I%@H�@Hb@G�@Gl�@GK�@G
=@F�@F�@F�R@F5?@F{@E�T@E��@E@E�h@E`B@E�@D��@DZ@D(�@C��@Cƨ@C��@C��@C"�@B�@B��@B��@B�!@B�\@BM�@B=q@B�@A�#@A�7@AG�@A&�@A%@A%@@��@@r�@@1'@?��@?��@?�P@?|�@?+@>��@>$�@>$�@=�@=`B@<�j@<�@<��@<z�@<�@;ƨ@;��@;�@;S�@;@:~�@:M�@9��@9��@9x�@97L@8�9@81'@7�w@7��@7|�@7K�@7�@6�y@6ȴ@6��@65?@5��@5`B@4��@4�j@4�D@4I�@49X@4�@3�F@333@3o@3@2��@2��@2n�@2=q@2J@1��@1��@1�7@17L@17L@1�@0�`@0Ĝ@0��@0�@0bN@0A�@0  @/|�@/;d@/�@.��@.�R@.ff@.5?@.@-�@-��@-@-�h@-`B@-?}@,�@,�@,I�@+�m@+t�@+C�@+@*��@*�\@*M�@)�#@)%@(��@(�@(bN@'�;@'��@'�@'\)@';d@'
=@&�y@&ȴ@&�+@&E�@&5?@&$�@%�T@%�T@%@%�-@%�@%�@%p�@%`B@%O�@%?}@%/@%/@%�@$�@$�D@#�
@#dZ@"�@"�!@!��@!%@ �u@ b@�w@�w@��@\)@+@�@�y@��@�+@v�@V@$�@�@�T@��@�@�@�@�@p�@O�@�@��@�@�j@j@ƨ@��@��@��@dZ@C�@33@o@�@��@��@��@n�@�@��@x�@7L@��@Ĝ@Ĝ@�9@�@bN@b@b@  @�w@�@l�@�@��@��@v�@5?@$�@@�T@�-@�@`B@?}@V@��@�@�@�/@��@�j@z�@Z@I�@9X@(�@1@��@��@�m@ƨ@�@dZ@33@33@o@��@��@�!@�!@��@��@~�@~�@n�@^5@=q@-@��@��@�^@�7@x�@�7@��@hs@7L@7L@�@��@bN@ �@  @�w@K�@��@��@v�@ff@5?@��@�-@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB{BM�BiyBr�Bq�BcTBcTBXBM�BL�BH�BbNBjBhsBdZBaHBZBW
BR�BO�BL�BK�BI�BI�BL�BO�BW
BVBQ�BYBL�B]/B`BB^5BZBT�BK�B8RB+B�BB��B�/B�B��B��B��BȴBB�-B��B��B�{B�+Bm�B\)BR�BJ�BF�B>wB9XB'�B�BB
�B
�#B
��B
�FB
��B
��B
��B
�hB
�B
s�B
e`B
ZB
O�B
A�B
+B
�B
�B
%B	�B	��B	B	��B	�=B	� B	|�B	u�B	k�B	aHB	[#B	O�B	?}B	&�B	�B	oB	PB	B��B�B�`B�/B��B��BĜB��B�qB�!B��B��B��B�hB��B�bB�B}�By�Bq�Br�Bx�Bu�Bs�Bq�Bn�BiyBiyBgmBffBffBe`BdZBaHB_;B^5B\)B[#BXBVBR�BQ�BM�BL�BK�BK�BJ�BI�BH�BF�BF�BD�BC�B>wB?}B;dB:^B:^B;dB8RB7LB5?B5?B49B0!B-B-B,B)�B'�B&�B(�B33B5?B6FB6FB49B/B.B-B,B.B.B0!B/B/B/B.B.B1'B1'B33B49B33B49B6FB<jB?}B>wB>wB?}B?}BA�B@�B@�B@�B@�BB�BB�BE�BG�BI�BM�BP�BS�BT�BW
BYB[#B\)B\)B_;BaHBaHBbNBdZBffBffBgmBiyBjBk�Bk�Bm�Bn�Bp�Br�Br�Bs�Bu�Bv�Bw�Bw�Bw�Bw�Bw�Bx�By�B|�B|�B�B�B�+B�+B�7B�7B�7B�DB�PB�VB�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�!B�FB�RB�wBÖBȴB��B��B��B��B��B�#B�/B�;B�BB�NB�`B�yB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	1B	DB	\B	oB	�B	�B	�B	�B	�B	!�B	"�B	%�B	%�B	'�B	(�B	)�B	,B	/B	1'B	2-B	5?B	9XB	:^B	:^B	;dB	<jB	?}B	@�B	A�B	E�B	H�B	I�B	M�B	P�B	Q�B	Q�B	Q�B	R�B	R�B	S�B	T�B	XB	ZB	\)B	]/B	^5B	^5B	_;B	dZB	hsB	iyB	k�B	m�B	m�B	p�B	r�B	t�B	u�B	y�B	z�B	|�B	}�B	~�B	�B	�B	�B	�+B	�7B	�DB	�PB	�bB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�LB	�LB	�XB	�dB	�jB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�`B	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
	7B

=B

=B
DB
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
YB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�"B�BmBN�Bi�Bs�Bs3BeBe�B[�BPbBN�BLBe�BlWBi�BgmBd�B]BYeBT�BRBN"BLJBJ�BLJBOBS&BY�BX+BT�BZ�BN�B^�BaHB_�B\CBYBO(B;B./B�B�B�LB�BںBҽB�"B�dB�#B�gB�MB�0B��B��B��BoiB]�BT,BK�BG�B?�B;�B*KB�B�B
�B
�~B
��B
��B
��B
�,B
�B
�uB
�3B
utB
f�B
[�B
RTB
C�B
,=B
 �B
�B
	�B	�hB	��B	�zB	��B	�DB	��B	~BB	w�B	m)B	c B	]~B	TB	CB	)�B	�B	B	\B	�B�XB�B�B��BּB�}B��B�GB�.B��B�kB�B��B�B�+B�B�3B� B{BraBs�By�Bv`Bt�Br�BoiBjeBi�BgRBf�Bf�Bf�Be�BbNB_�B^�B]/B\BX�BW�BT,BS@BN�BMBK�BLBKxBJ�BI�BG�BHBE�BD�B?�B@�B;�B:�B;�B<�B9$B8�B6�B7LB5�B0�B-�B.�B,�B*�B(sB(
B+B4B5�B72B7�B5ZB/�B.�B./B-CB.�B.�B0�B/�B/�B0B/B/B1�B1�B49B5tB4�B5?B6�B=VB@OB?�B@ BAB@�BB�BA;B@�B@�BAUBCaBC�BFtBH�BJ�BN�BQ�BT,BUMBW�BYeB[qB\CB\�B_�BaHBa�Bb�Bd�Bf�Bf�Bg�Bi�Bj�Bk�Bk�Bn/Bn�BqBsBr�Bs�Bu�Bv�Bw�Bw�Bw�Bw�Bw�Bx�Bz*B}B|�B�B�-B�EB�B�B�B�7B�^B��B��B��B�oB��B��B�
B�xB�B��B�4B�B�@B�8B�_B�=B�=B��B��B��B��BðBȴB��BοB�B��B�gB�=B�B�;B�BB�NB��B��B��B�B�B�B��B��B��B��B��B�B�HB	oB	-B	3B	B	�B	)B	\B	�B	�B	_B	�B	�B	�B	!�B	"�B	%�B	&B	(
B	(�B	)�B	+�B	/B	1AB	2B	5B	9$B	:DB	:DB	;JB	<PB	?cB	@OB	A�B	E�B	H�B	I�B	M�B	P�B	Q�B	Q�B	Q�B	R�B	SB	S�B	T�B	XB	ZB	[�B	]B	^B	^B	_pB	d@B	hXB	i_B	kkB	mwB	m�B	p�B	r|B	t�B	u�B	y�B	z�B	|�B	}�B	~�B	��B	��B	�3B	�B	�B	�)B	�PB	�.B	�.B	�NB	�[B	�aB	��B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	� B	�AB	�9B	�%B	�B	�B	�LB	�$B	�JB	�PB	�<B	�]B	�}B	�UB	�[B	ÖB	āB	ǔB	�lB	ɠB	ɠB	˒B	ˬB	̳B	͟B	͹B	οB	��B	��B	�B	��B	�B	��B	��B	��B	��B	�1B	�=B	�B	�B	�B	�B	�B	�-B	�4B	�4B	�NB	�nB	�nB	�zB	�RB	�>B	�>B	�_B	�_B	�yB	�eB	�eB	�eB	�QB	�B	�kB	�WB	�WB	�qB	�wB	�wB	�B	�iB	�oB	�vB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	��B
B
�B
�B
B
�B
�B
�B
�B
9B
�B
�B
�B
%B
+B
�B
�B
	B

#B

#B
)B
0B
B
B
0B
6B
6B
6B
"B
"B
<B
VB
\B
BB
BB
HB
bB
hB
TB
TB
@B
@B
[B
aB
�B
�B
�B
�B
�B
yB
eB
B
�B
�B
WB
�B
�B
�B
�B
�B
qB
�B
�B
~B
dB
pB
 vB
 vB
!�B
!�B
!�B
!�B
!�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+B
,"B
-B
,�B
-�B
.B
.B
.�B
/ B
/�B
/�B
/�B
0!B
1B
1B
1B
0�B
0�B
2B
1�B
2B
1�B
2B
1�B
2B
2B
1�B
2�B
2�B
2B
2�B
2�B
4B
4B
4B
5B
5%B
6+B
6+B
72B
7fB
88B
8B
8B
8B
8B
9>B
9>B
9>B
9$B
9$B
:*B
:*B
:DB
:DB
:DB
;0B
;B
;0B
;JB
;JB
;0B
;JB
;0B
<PB
<PB
<PB
<PB
<PB
<PB
=<B
=<B
=<B
>]B
>BB
>BB
>]B
?HB
?HB
?.B
?cB
?HB
?HB
@iB
@iB
@iB
@iB
AoB
AoB
AoB
AUB
A�B
B[B
B[B
B[B
B[B
B[B
BuB
CaB
C{B
C{B
C{B
CaB
CaB
CaB
CaB
D�B
D�B
DgB
D�B
DgB
EmB
EmB
E�B
E�B
EmB
EmB
E�B
E�B
F�B
FtB
FtB
F�B
GzB
GzB
GzB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
U�B
U�B
U�B
U�B
V�B
W
B
W$B
W�B
W�B
W�B
XB
X�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
[	B
[	B
[	B
[	B
[=B
[=B
[#B
\B
[�B
[�B
[�B
\B
\�B
\�B
]B
]B
^B
^B
^B
^B
^B
^B
^B
^B
_B
_B
_B
^�B
_B
_B
`B
`B
`B
`'B
`BB
aB
aB
`�B
aB
aB
aB
aB
bB
b4B
bB
b4B
b4B
b4B
c B
c:B
c B
c B
d@B
d&B
d&B
d&B
d&B
e,B
e,B
eFB
e,B
e,B
eFB
f2B
fLB
f2B
fLB
g8B
gB
gRB
g8B
g8B
g8B
gRB
gRB
h>B
h$B
h>B
h$B
h$B
h>B
h>B
h>B
hXB
h$B
h>B
h>B
iDB
i*B
iDB
iDB
iDB
i_B
i_B
i_B
jKB
jeB
jKB
jKB
jKB
j0B
j0B
jKB
jKB
j0B
jKB
jKB
jKB
k6B
kQB
kQB
k6B
kkB
k6B
kQB
lWB
mwB
mwB
m]B
m]B
n}B
ncB
m]B
n}B
ncB
n�B
n}B
oiB
n}B
n}B
n}B
n}B
o�B
o�B
oi111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.51(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202012180036032020121800360320201218003603202306231726302023062317263020230623172630202012190034532020121900345320201219003453  JA  ARFMdecpA19c                                                                20201213063921  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201212213943  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201212213944  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201212213944  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201212213944  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201212213944  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201212213944  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201212213944  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201212213945  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201212213945                      G�O�G�O�G�O�                JA  ARUP                                                                        20201212215145                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20201213153342  CV  JULD            G�O�G�O�F�s>                JM  ARCAJMQC2.0                                                                 20201217153603  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201217153603  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20201218153453  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082630  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                