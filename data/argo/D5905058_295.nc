CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-02-11T06:43:01Z creation;2021-02-11T06:43:03Z conversion to V3.1;2023-06-29T05:47:16Z update;     
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
resolution        =���   axis      Z        t  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ix   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  MX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210211064301  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              'A   JA  I2_0675_295                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�]|,/ 1   @�]|,/ @6�����>�b�F�]c�8   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�z�A��A*=qAJ=qAj=qA��A��A��A��A��A��A��A��B�\B
�\B�\B�\B"�\B*�\B2�\B:�\BB�\BJ�\BR�\BZ�\Bb�\Bj�\Br�\Bz�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�EC�Q�C�^�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�^�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D	(�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'(�D'�\D((�D(��D)(�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3(�D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<(�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD"�DD��DE(�DE��DF(�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR��DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX��DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db"�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk(�Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt��Du(�Du��Dv(�Dv��Dw(�Dw��Dx(�Dx��Dy(�Dy��Dz(�Dz��D{(�D{��D|(�D|��D}(�D}��D~(�D~��D(�D��D�{D�QHD��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D��D�W�D��{D��{D�{D�QHD��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D��D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D{D��{D�{D�T{DÔ{D��{D�{D�T{DĔ{D��{D�{D�T{DŔ{D��{D�{D�T{DƔ{D��{D�{D�T{Dǔ{D��{D�{D�T{DȔ{D��{D�{D�T{Dɔ{D��{D�{D�T{Dʔ{D��{D�{D�T{D˔{D��{D�{D�T{D̔{D��{D�{D�T{D͔{D��{D�{D�T{DΔ{D��{D�{D�T{Dϔ{D��{D�{D�T{DД{D��{D�{D�T{Dє{D��{D�{D�T{DҔ{D��{D�{D�T{DӔ{D��{D�{D�T{DԔ{D��{D�{D�T{DՔ{D��{D�{D�T{D֔{D��{D�{D�T{Dה{D��{D�{D�T{Dؔ{D��{D�{D�T{Dٔ{D��{D�{D�T{Dڔ{D��{D�{D�T{D۔{D��{D�{D�T{Dܔ{D��{D�{D�T{Dݔ{D��{D�{D�T{Dޔ{D��{D�{D�T{Dߔ{D��{D�{D�T{D��{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�HD��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��HD�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�W�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�;dA�=qA�=qA�;dA�5?A�+A�(�A�(�A�(�A�+A�+A�+A�+A�(�A�+A�+A�-A�-A�-A�/A�/A�/A�1'A�1'A�1'A�33A�1'A�1'A�+A�"�A� �A��A��A��A�VA�  A��TA���A��A�p�A�XA�7LA�A��;A��PA�M�A�$�A���A��RA��A��A�x�A��A�"�A�VA�v�A�VA���A�\)A�/A���A��!A�M�A�E�A���A�$�A��A��A�"�A�A���A�~�A�+A�|�A���A�ĜA��uA��9A�
=A���A���A��A��`A�n�A��A�|�A���A�K�A�ffA�JA�ĜA���A�-A��-A��mA�"�A�S�A���A�x�A�I�A��A�ȴA��A��A� �A���A�`BA���A���A���A�+A���A�+A�E�A�%A~�yAz�Ax��Aw�AuVAp��Am��Aj�AhM�Af �Ad1Aa�#A^M�A[ƨA[%AZZAY��AY�7AY;dAWdZAU�AS�AP�RAN��AM��ALQ�AK�AJjAG�ADz�AC��AChsAA��A?�A>ȴA<��A;C�A:��A:1A8�uA7t�A6��A5?}A4jA3?}A2bNA1�A0�9A0-A/�A-�PA*bNA(v�A'?}A&��A%��A$�A$ �A#��A#l�A!��A!hsA ��A r�A�RAȴA�
AK�A��A(�A=qAC�AĜA��A1AA1'A?}AA��A�AƨAt�A
�jA
A	dZA�;A�hA�7A;dA�A�#A�A�jA�A�A$�A��Ax�A"�A^5A�A Ĝ@�C�@�V@���@�b@��@��+@���@�7L@��@�V@��u@���@�;d@�o@���@��@�Z@�33@�R@���@�dZ@�G�@��`@�+@�x�@�Z@��@��@�9@�@��#@�G�@ߥ�@��@ܓu@�ƨ@�33@�`B@��@��;@ҟ�@���@У�@��;@�33@�{@�%@�  @��H@ʸR@ʟ�@�n�@���@ȓu@��y@ũ�@�Z@�  @��
@þw@Ý�@Å@��@�O�@��@�\)@���@�n�@�$�@��-@�7L@�b@���@�x�@���@��/@��/@���@��j@���@�o@��@���@�1'@���@��H@��^@���@�9X@��P@�ȴ@���@�hs@�z�@�"�@�5?@�V@���@��@��F@�l�@��@�M�@���@�hs@��/@���@���@�r�@�I�@��@�S�@�@���@���@�@��^@���@�p�@�&�@���@�r�@�b@��P@�K�@�33@��@��@�o@�
=@���@��y@��y@��@��@���@���@�-@��h@�p�@�O�@�&�@���@�Ĝ@��j@�bN@�b@�1@���@��m@�ƨ@�|�@���@���@�M�@��@��T@��@�`B@��@��j@�j@�Z@�I�@�9X@�1'@��w@�S�@��@��y@���@��R@��R@���@�~�@�v�@�=q@��@�@��@��@���@��@��`@�z�@�bN@�1'@�  @��;@��;@�ƨ@���@�K�@�o@�o@�
=@�ȴ@�5?@��@���@��h@�O�@��@��`@��@�bN@���@���@��@�dZ@�
=@���@�^5@�@��@��#@�@��h@�7L@��`@��9@��@�bN@�I�@�A�@�A�@�9X@�1'@�(�@�(�@��;@���@�S�@�+@��@�ȴ@���@���@��+@�M�@��@���@�7L@��@��`@���@��D@�j@�Z@�1'@���@��m@���@�|�@�\)@�
=@���@��+@�-@�J@���@��#@���@��h@��@��`@��/@���@��@��u@�Z@�1'@�1@��;@���@�33@��y@��+@�-@��#@���@���@�`B@��@��9@�I�@�9X@�1'@�b@�@l�@�@~�R@~V@~@}��@}O�@}/@|�/@|z�@|9X@|�@|�@{��@{�F@{��@{33@z��@z�!@zn�@y�@y7L@xĜ@xbN@x �@w�;@w��@wl�@w;d@w
=@v�R@v��@vff@vV@vE�@vE�@v5?@v$�@v@u�T@u@uO�@uV@t��@t1@s�m@s33@r��@r��@r-@r�@q��@q�#@q�7@q7L@p��@p1'@o�P@n��@nE�@m�h@l�@lj@l(�@k��@kƨ@k�@kC�@j�H@j�!@j�\@i�@i��@i�@h��@g�;@g�@gK�@f�@fv�@f{@eO�@dZ@c��@cƨ@c��@c�@co@b�!@b-@a�7@aX@a�@`Ĝ@`�9@`��@`r�@_�;@_�@^ȴ@^�+@^�+@^v�@^ff@^V@^E�@^5?@]��@\�j@\�D@\1@[dZ@["�@Z�@Z��@Z�!@Z~�@ZM�@ZJ@Y��@Y��@Yx�@Y7L@Y%@XĜ@X�@W�@W�@Wl�@WK�@W+@V�@V$�@U�@U`B@U?}@T�@Tz�@TZ@T�@S�m@S�F@SS�@So@R��@R�\@R=q@Q�^@QX@Q%@P�`@P��@PĜ@P�u@O�@O\)@O+@N�@Nȴ@N��@N$�@M��@M@M�h@M?}@L�j@L1@K�
@Kƨ@Kt�@K33@J�!@J=q@J�@I��@I��@I��@Ihs@I&�@I%@H��@H�`@HĜ@H��@H��@Hr�@HQ�@HA�@G��@F�@FE�@F$�@F@E�@E@E`B@E�@D��@Dj@DI�@DI�@D(�@D1@C�
@C�F@Ct�@Co@B�H@A��@A�@A%@@�`@@��@@��@@��@@�@@Q�@@A�@@  @?�w@?�P@?�P@?K�@>�y@>�R@>�+@>5?@=�T@=��@<��@<��@<�@<�D@<I�@;�m@;t�@;S�@;33@;@:��@:��@:n�@:=q@:-@9��@9�@9��@9��@9hs@9G�@9�@8��@8�@8  @7�;@7�;@7��@7�w@7�P@7K�@7
=@6�@6�R@6��@5�T@5��@5O�@5/@4�j@4z�@4Z@4Z@4(�@4�@3�@3C�@3o@2�@2��@2^5@1��@1x�@1X@1�@0��@0�9@0bN@0 �@/��@/|�@/;d@.�@.�R@.��@.��@.��@.v�@.5?@-�@-�h@-/@,��@,�/@,�j@,j@,�@+�F@+��@+t�@+t�@+t�@+dZ@+C�@+33@+@*��@*=q@)�#@)��@)X@)�@(��@(��@(r�@(b@'�w@'\)@'�@&�@&��@&�+@&ff@&V@&E�@&$�@%��@%��@%p�@%O�@%?}@$��@$�j@$I�@$(�@$�@$�@$1@#ƨ@#t�@#o@"�@"��@"J@!�@!��@!��@!x�@!&�@ �`@ �@ bN@ bN@ 1'@�@��@l�@;d@�@��@�@ȴ@��@v�@V@�T@��@��@�h@?}@�/@�@z�@(�@�
@�F@�F@�F@��@t�@"�@�@��@��@��@�\@n�@n�@=q@�@��@x�@G�@&�@�@�@��@�9@�u@�@Q�@ �@b@��@;d@�@
=@�y@�R@��@V@{@�-@�@p�@O�@�@V@�@��@�@z�@z�@Z@I�@1@�
@ƨ@ƨ@��@�@dZ@C�@�@��@��@~�@n�@n�@n�@^5@^5@M�@=q@-@��@��@��@G�@7L@&�@�@%@�`@��@�@A�@ �@��@�P@+@��@ȴ@�+@E�@@�T@�h@�@p�@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�;dA�=qA�=qA�;dA�5?A�+A�(�A�(�A�(�A�+A�+A�+A�+A�(�A�+A�+A�-A�-A�-A�/A�/A�/A�1'A�1'A�1'A�33A�1'A�1'A�+A�"�A� �A��A��A��A�VA�  A��TA���A��A�p�A�XA�7LA�A��;A��PA�M�A�$�A���A��RA��A��A�x�A��A�"�A�VA�v�A�VA���A�\)A�/A���A��!A�M�A�E�A���A�$�A��A��A�"�A�A���A�~�A�+A�|�A���A�ĜA��uA��9A�
=A���A���A��A��`A�n�A��A�|�A���A�K�A�ffA�JA�ĜA���A�-A��-A��mA�"�A�S�A���A�x�A�I�A��A�ȴA��A��A� �A���A�`BA���A���A���A�+A���A�+A�E�A�%A~�yAz�Ax��Aw�AuVAp��Am��Aj�AhM�Af �Ad1Aa�#A^M�A[ƨA[%AZZAY��AY�7AY;dAWdZAU�AS�AP�RAN��AM��ALQ�AK�AJjAG�ADz�AC��AChsAA��A?�A>ȴA<��A;C�A:��A:1A8�uA7t�A6��A5?}A4jA3?}A2bNA1�A0�9A0-A/�A-�PA*bNA(v�A'?}A&��A%��A$�A$ �A#��A#l�A!��A!hsA ��A r�A�RAȴA�
AK�A��A(�A=qAC�AĜA��A1AA1'A?}AA��A�AƨAt�A
�jA
A	dZA�;A�hA�7A;dA�A�#A�A�jA�A�A$�A��Ax�A"�A^5A�A Ĝ@�C�@�V@���@�b@��@��+@���@�7L@��@�V@��u@���@�;d@�o@���@��@�Z@�33@�R@���@�dZ@�G�@��`@�+@�x�@�Z@��@��@�9@�@��#@�G�@ߥ�@��@ܓu@�ƨ@�33@�`B@��@��;@ҟ�@���@У�@��;@�33@�{@�%@�  @��H@ʸR@ʟ�@�n�@���@ȓu@��y@ũ�@�Z@�  @��
@þw@Ý�@Å@��@�O�@��@�\)@���@�n�@�$�@��-@�7L@�b@���@�x�@���@��/@��/@���@��j@���@�o@��@���@�1'@���@��H@��^@���@�9X@��P@�ȴ@���@�hs@�z�@�"�@�5?@�V@���@��@��F@�l�@��@�M�@���@�hs@��/@���@���@�r�@�I�@��@�S�@�@���@���@�@��^@���@�p�@�&�@���@�r�@�b@��P@�K�@�33@��@��@�o@�
=@���@��y@��y@��@��@���@���@�-@��h@�p�@�O�@�&�@���@�Ĝ@��j@�bN@�b@�1@���@��m@�ƨ@�|�@���@���@�M�@��@��T@��@�`B@��@��j@�j@�Z@�I�@�9X@�1'@��w@�S�@��@��y@���@��R@��R@���@�~�@�v�@�=q@��@�@��@��@���@��@��`@�z�@�bN@�1'@�  @��;@��;@�ƨ@���@�K�@�o@�o@�
=@�ȴ@�5?@��@���@��h@�O�@��@��`@��@�bN@���@���@��@�dZ@�
=@���@�^5@�@��@��#@�@��h@�7L@��`@��9@��@�bN@�I�@�A�@�A�@�9X@�1'@�(�@�(�@��;@���@�S�@�+@��@�ȴ@���@���@��+@�M�@��@���@�7L@��@��`@���@��D@�j@�Z@�1'@���@��m@���@�|�@�\)@�
=@���@��+@�-@�J@���@��#@���@��h@��@��`@��/@���@��@��u@�Z@�1'@�1@��;@���@�33@��y@��+@�-@��#@���@���@�`B@��@��9@�I�@�9X@�1'@�b@�@l�@�@~�R@~V@~@}��@}O�@}/@|�/@|z�@|9X@|�@|�@{��@{�F@{��@{33@z��@z�!@zn�@y�@y7L@xĜ@xbN@x �@w�;@w��@wl�@w;d@w
=@v�R@v��@vff@vV@vE�@vE�@v5?@v$�@v@u�T@u@uO�@uV@t��@t1@s�m@s33@r��@r��@r-@r�@q��@q�#@q�7@q7L@p��@p1'@o�P@n��@nE�@m�h@l�@lj@l(�@k��@kƨ@k�@kC�@j�H@j�!@j�\@i�@i��@i�@h��@g�;@g�@gK�@f�@fv�@f{@eO�@dZ@c��@cƨ@c��@c�@co@b�!@b-@a�7@aX@a�@`Ĝ@`�9@`��@`r�@_�;@_�@^ȴ@^�+@^�+@^v�@^ff@^V@^E�@^5?@]��@\�j@\�D@\1@[dZ@["�@Z�@Z��@Z�!@Z~�@ZM�@ZJ@Y��@Y��@Yx�@Y7L@Y%@XĜ@X�@W�@W�@Wl�@WK�@W+@V�@V$�@U�@U`B@U?}@T�@Tz�@TZ@T�@S�m@S�F@SS�@So@R��@R�\@R=q@Q�^@QX@Q%@P�`@P��@PĜ@P�u@O�@O\)@O+@N�@Nȴ@N��@N$�@M��@M@M�h@M?}@L�j@L1@K�
@Kƨ@Kt�@K33@J�!@J=q@J�@I��@I��@I��@Ihs@I&�@I%@H��@H�`@HĜ@H��@H��@Hr�@HQ�@HA�@G��@F�@FE�@F$�@F@E�@E@E`B@E�@D��@Dj@DI�@DI�@D(�@D1@C�
@C�F@Ct�@Co@B�H@A��@A�@A%@@�`@@��@@��@@��@@�@@Q�@@A�@@  @?�w@?�P@?�P@?K�@>�y@>�R@>�+@>5?@=�T@=��@<��@<��@<�@<�D@<I�@;�m@;t�@;S�@;33@;@:��@:��@:n�@:=q@:-@9��@9�@9��@9��@9hs@9G�@9�@8��@8�@8  @7�;@7�;@7��@7�w@7�P@7K�@7
=@6�@6�R@6��@5�T@5��@5O�@5/@4�j@4z�@4Z@4Z@4(�@4�@3�@3C�@3o@2�@2��@2^5@1��@1x�@1X@1�@0��@0�9@0bN@0 �@/��@/|�@/;d@.�@.�R@.��@.��@.��@.v�@.5?@-�@-�h@-/@,��@,�/@,�j@,j@,�@+�F@+��@+t�@+t�@+t�@+dZ@+C�@+33@+@*��@*=q@)�#@)��@)X@)�@(��@(��@(r�@(b@'�w@'\)@'�@&�@&��@&�+@&ff@&V@&E�@&$�@%��@%��@%p�@%O�@%?}@$��@$�j@$I�@$(�@$�@$�@$1@#ƨ@#t�@#o@"�@"��@"J@!�@!��@!��@!x�@!&�@ �`@ �@ bN@ bN@ 1'@�@��@l�@;d@�@��@�@ȴ@��@v�@V@�T@��@��@�h@?}@�/@�@z�@(�@�
@�F@�F@�F@��@t�@"�@�@��@��@��@�\@n�@n�@=q@�@��@x�@G�@&�@�@�@��@�9@�u@�@Q�@ �@b@��@;d@�@
=@�y@�R@��@V@{@�-@�@p�@O�@�@V@�@��@�@z�@z�@Z@I�@1@�
@ƨ@ƨ@��@�@dZ@C�@�@��@��@~�@n�@n�@n�@^5@^5@M�@=q@-@��@��@��@G�@7L@&�@�@%@�`@��@�@A�@ �@��@�P@+@��@ȴ@�+@E�@@�T@�h@�@p�@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�=B�=B�=B�=B�=B�=B�7B�7B�7B�7B�7B�7B�7B�7B�=B�=B�DB�DB�DB�JB�PB�VB�VB�bB�hB�hB�oB�{B��B��B��B��B��B��B��B��B�B�-B�LB�^B��BÖBŢBɺB��B��B��B��B��B�ZB�sB�B�B��B%BJBbB�B�B"�B)�B.B-B0!B0!B0!B1'B0!B6FB8RB;dB:^B8RB5?B/B(�B"�B�B�B{BuB��B�yB�BB�B��B��B��B�'B��B��B��B��B�bB�BgmBYBN�BF�B@�B>wB33B'�B�BB
�TB
��B
B
�RB
��B
��B
�B
v�B
l�B
_;B
L�B
7LB
�B
	7B	��B	�B	��B	�^B	��B	��B	�=B	}�B	q�B	^5B	M�B	H�B	C�B	@�B	=qB	:^B	49B	&�B	�B	VB	  B��B�B�B�sB�HB��BȴBȴB��B�FB�'B�B��B��B��B��B��B��B��B��B�uB�\B�JB�=B�1B�+B�Bw�Bo�Bk�BiyBffBe`BcTBbNB`BBaHB]/B\)B\)B[#B[#BW
BW
BVBS�BQ�BM�BK�BH�BD�BB�B?}B?}B>wB:^B<jB:^B9XB9XB7LB8RB;dB:^B:^B9XB7LB6FB0!B0!B0!B0!B/B.B.B-B.B.B.B/B.B1'B33B33B2-B2-B8RB;dB=qB?}B?}B>wB=qB=qB=qB<jB<jB;dB<jB;dB6FB6FB6FB49B33B33B49B49B49B49B49B6FB7LB8RB7LB7LB:^B?}BA�BC�BE�BH�BI�BI�BK�BM�BQ�BT�BT�BT�BVBW
BZB`BBdZBiyBiyBjBjBjBjBl�Bp�Bq�Bt�Bt�Bv�Bw�By�Bz�B~�B�B�+B�=B�DB�DB�DB�DB�bB�oB��B��B��B��B��B��B��B�B�B�!B�3B�3B�FB�wBÖBȴB��B��B��B��B�)B�NB�sB�B�B�B�B�B�B�B��B��B��B��B	  B	1B	JB	\B	hB	{B	�B	�B	$�B	'�B	'�B	(�B	(�B	(�B	)�B	)�B	+B	+B	+B	+B	,B	-B	1'B	6FB	7LB	8RB	:^B	<jB	<jB	<jB	?}B	A�B	A�B	B�B	B�B	C�B	F�B	L�B	O�B	Q�B	R�B	T�B	YB	ZB	]/B	`BB	bNB	bNB	cTB	cTB	dZB	iyB	n�B	r�B	t�B	u�B	v�B	v�B	w�B	x�B	z�B	{�B	|�B	}�B	~�B	� B	�B	�B	�1B	�DB	�JB	�VB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�?B	�LB	�XB	�jB	�qB	�wB	�wB	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
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

=B
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
hB
hB
hB
hB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
)�B
+B
)�B
)�B
)�B
+B
+B
,B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
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
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
<jB
<jB
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
<jB
<jB
=qB
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
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
D�B
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
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
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
[#B
[#B
\)B
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
_;B
_;B
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
aHB
aHB
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
ffB
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
iyB
iyB
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�	B�	B�	B�	B�#B�#B�B�B�B�B�B�B�B�B�	B�	B�B�B�B�B�B�"B�"B�.B�4B�4B�:B�FB�gB�B��B��B��B��B��B��B�)B�-B�fB��B��BðB��B��B�PB�HB�}BуB�YB�B�eB�B�B��B�B<B�B�B~B$�B*�B.IB.}B3hB1B1�B2�B33B88B:*B;�B:�B9$B6�B0�B+�B%,B�BB�B�B�]B�B�B�#B�&B��B��B��B��B�;B�=B�1B��B�MBi*BZ�BO�BGEBA B@B4�B)�B5B�B
��B
��B
��B
�xB
��B
�QB
�[B
x8B
n�B
bhB
P�B
;0B
B
DB
;B	�B	�sB	�B	��B	�B	��B	��B	uZB	`�B	N�B	I7B	DB	@�B	>]B	<�B	6�B	)_B	5B	}B	;B�0B��B�OB�kB�ZBбBɆBʦBB�B�3B��B��B��B�ZB� B��B�B��B��B�aB�HB�B�B��B�lB�MBy�Bp�Bl=Bj�Bg�Be�Bc�Bb�Ba�Ba�B]�B]/B^5B]B\BW�BW�BV�BU�BR�BN�BMBJ�BE�BC�B@�BA�B?�B;B=�B:�B:DB:*B8B9�B;B:^B:�B9�B88B7B0!B0;B0;B0oB/�B.IB.}B-�B/ B/B/5B/�B.}B1�B3�B3MB2aB2aB88B;0B=�B?�B?�B>wB=�B>(B=�B<�B<�B<B=�B<PB6�B7LB72B4�B3�B3�B4�B4�B5B4�B5B72B7�B8�B7�B8�B<6B@�BB'BC�BF%BIBJ	BJ=BLJBNVBRTBT�BT�BUBVmBW�BZ�B`�Bd�Bi_Bi_BjeBjeBjBj�BmCBp�Br-Bt�Bt�Bv�BxBz*B{dB�B�{B�+B�	B�B�)B�DB��B��B��B�
B��B��B�!B�NB�2B�*B�QB�WB��B�MB��B��B��B��B��B��B��B��B�B�CB�hB�sB�B�wB�cB��B�oB�B��B��B��B��B�B��B	B	0B	\B	hB	{B	�B	�B	$�B	'�B	'�B	(�B	(�B	(�B	)�B	)�B	*�B	*�B	*�B	*�B	+�B	-)B	1'B	6B	7B	88B	:DB	<6B	<6B	<jB	?cB	AUB	AUB	B[B	BuB	C�B	F�B	L�B	O�B	Q�B	R�B	T�B	X�B	ZB	]/B	`'B	bB	bB	cB	c B	dtB	iyB	n}B	r|B	tnB	u�B	v�B	v�B	w�B	x�B	z�B	{�B	|�B	}�B	~�B	� B	��B	�3B	�1B	�B	�B	�"B	�B	�.B	�:B	�[B	�{B	�YB	�YB	�EB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�?B	�LB	�XB	�PB	�"B	�BB	�BB	�HB	��B	�[B	�gB	�mB	�YB	�zB	�fB	�KB	ȀB	ȀB	�fB	ȀB	ȚB	ʦB	˒B	͟B	ΥB	ΥB	ЗB	бB	ЗB	��B	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	��B	��B	��B	�;B	�'B	�-B	�:B	�&B	�B	�,B	�LB	�2B	�LB	�*B	�DB	�DB	�DB	�KB	�eB	�QB	�kB	�WB	�wB	�B	�B	�B	�B	�B	�|B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
	�B

	B

�B

	B
B
B
)B
B
0B
0B
6B
VB
BB
\B
4B
4B
4B
4B
4B
:B
TB
@B
@B
aB
MB
gB
SB
mB
YB
sB
yB
yB
_B
�B
�B
qB
WB
xB
xB
�B
xB
~B
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
pB
pB
pB
pB
pB
 \B
�B
�B
 �B
!|B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
)�B
*�B
)�B
)�B
)�B
*�B
*�B
+�B
,�B
,�B
,�B
-�B
.�B
.�B
.�B
/�B
0B
/�B
0�B
0�B
1B
1B
1�B
1�B
2�B
2�B
2�B
2�B
33B
4B
4B
4B
5B
4�B
5B
6B
6B
6B
6+B
6+B
7B
7B
7B
8B
8B
88B
9$B
9$B
9$B
:B
:*B
:*B
:*B
:*B
:B
:B
:*B
:*B
;B
;B
;B
;0B
;JB
;dB
;0B
<6B
<6B
<B
<6B
<6B
<6B
<6B
<B
<B
:�B
;0B
;0B
<6B
<6B
<B
=VB
=VB
<jB
<PB
;0B
;0B
:�B
;B
;0B
;0B
;0B
<B
<6B
<6B
<B
<B
<6B
<6B
<6B
=<B
=<B
="B
=<B
>BB
>BB
>BB
>BB
?HB
?cB
?cB
@OB
@OB
@4B
@4B
@OB
A;B
AUB
AUB
A;B
BAB
B[B
B[B
B[B
CaB
CaB
CaB
CaB
C{B
DgB
DMB
DMB
DgB
DgB
DgB
EmB
EmB
EmB
EmB
EmB
EmB
ESB
FtB
F�B
FtB
GEB
G_B
G_B
G_B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
IlB
J�B
J�B
J�B
J�B
KxB
K�B
K�B
K�B
L�B
L�B
L�B
LdB
L~B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
^B
^�B
^�B
^�B
_B
^�B
^�B
_B
_B
_�B
`B
`B
`B
_�B
`B
`B
_�B
_�B
aB
aB
`�B
`�B
aB
aB
aB
`�B
aB
bB
bB
bB
c B
cB
cB
cB
d&B
d&B
d&B
d&B
d&B
e,B
eB
e,B
e,B
e�B
f2B
fB
f2B
f2B
gB
g8B
g8B
g8B
g8B
gB
gB
gB
h$B
h>B
h>B
h>B
iDB
iDB
i*B
i*B
iB
i*B
i*B
i*B
i*B
iB
j0B
jKB
jKB
jKB
jKB
jB
j0B
j0B
j0B
jKB
j0B
kQB
kQB
kQB
k6B
kQB
kQB
lWB
lWB
lWB
m]B
mCB
m]B
m]B
nIB
nIB
nIB
nI11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.64(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202102170034092021021700340920210217003409202306231727392023062317273920230623172739202102180021372021021800213720210218002137  JA  ARFMdecpA19c                                                                20210211154258  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210211064301  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210211064303  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210211064303  QCP$                G�O�G�O�G�O�               CJA  ARGQrqcppo_c                                                                20210211064303  QCF$                G�O�G�O�G�O�               CJA  ARGQrqcpt19d                                                                20210211064303  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210211064303  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210211064303  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210211064303  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210211064303  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210211064303                      G�O�G�O�G�O�                JA  ARUP                                                                        20210211065208                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210211153323  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20210211153323  CV  JULD_LOCATION   G�O�G�O�It#�                JM  ARGQJMQC2.0                                                                 20210211153323  CV  LATITUDE        G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210211153323  CV  LONGITUDE       G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210211153323  CV  POSITION_QC     G�O�G�O�A                  JM  ARCAJMQC2.0                                                                 20210216153409  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210216153409  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210217152137  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082739  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                