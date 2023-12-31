CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-19T03:35:30Z creation;2018-09-19T03:35:32Z conversion to V3.1;2019-12-23T06:14:57Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        D  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ʴ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180919033530  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ZA   JA  I2_0675_090                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؂��tn�1   @؂�m�5 @7t�/���cJh	ԕ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  @���A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�z�A��A*=qAJ=qAj=qA��A��A�Q�A��A��A��A��A��B�\B
�\B�\B�\B"�\B*�\B2�\B:�\BB�\BJ�\BR�\BZ�\Bb�\Bj�\Br�\Bz�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch�qCj��Cl��Cn�=Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�^�C�Q�C�Q�C�Q�C�Q�C�Q�C�^�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�^�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D	"�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'(�D'��D((�D(��D)(�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3(�D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<(�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD(�DD��DE(�DE��DF(�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR��DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX��DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db(�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk(�Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt��Du(�Du��Dv(�Dv��Dw(�Dw��Dx(�Dx��Dy(�Dy��Dz(�Dz��D{(�D{��D|(�D|��D}(�D}��D~(�D~��D(�D��D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D{D��{D�{D�T{DÔ{D��{D�{D�T{DĔ{D��{D�{D�T{DŔ{D��{D�{D�T{DƔ{D��{D�{D�T{Dǔ{D��{D�{D�T{DȔ{D��{D�{D�T{Dɔ{D��{D�{D�T{Dʔ{D��{D�{D�T{D˔{D��{D�{D�T{D̔{D��{D�{D�T{D͔{D��{D�{D�T{DΔ{D��{D�{D�T{Dϔ{D��{D�{D�T{DД{D��{D�{D�T{Dє{D��{D�{D�T{DҔ{D��{D�{D�T{DӔ{D��{D�{D�T{DԔ{D��{D�{D�T{DՔ{D��{D�{D�T{D֔{D��{D�{D�T{Dה{D��{D�{D�T{Dؔ{D��{D�{D�T{Dٔ{D��{D�{D�T{Dڔ{D��{D�{D�T{D۔{D��{D�{D�T{Dܔ{D��{D�{D�T{Dݔ{D��{D�{D�T{Dޔ{D��{D�{D�T{Dߔ{D��{D�{D�T{D��{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�W�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��AϮA�M�A���A���AήA΍PA�p�A�ZA�O�A�K�A�A�A�;dA�9XA�9XA�;dA�9XA�9XA�9XA�9XA�9XA�;dA�7LA�33A�/A�
=A�{A��A�K�A�;dA��A�t�A�VA��A�
=A�z�A��A���A�K�A���A�|�A��HA���A� �A���A�1'A��mA�/A��;A���A���A�{A��A�K�A�VA�v�A��FA��
A��A��uA�~�A���A�7LA��!A��TA�=qA��A��TA���A�;dA�A��A��wA�I�A��7A��A�?}A���A� �A�%A���A���A�n�A��A��`A��#A��DA���A�Q�A���A��A�n�A��TA��A�1'A���A�l�A���A��A��HA���A�oA�n�A���A��A�  A�Q�A}XAz�HAy33Av-AtȴArZAo;dAm��Am&�Al�AlȴAk�#AjAi|�Ah��Ah��Ah1'AeO�Ac�AbM�A_x�A^1A]�A\bNA[�7AZ��AYx�AW�;AW\)AUAT�AR�uAQ�7AOAK�AI��AG�;AE��AE�AEt�AE;dAD5?AB1A?�;A>VA> �A=�A=�^A=�^A=�A;�A:ZA9XA8�RA7��A6��A5��A5+A4~�A2��A1��A0�+A/;dA-��A-\)A,�A*jA)��A(=qA%oA"��A!\)A �HA��A�A�!A~�AdZA-A�#A�A��A�FA�A��AE�A�mA`BA  A��A��A��Az�A=qA�#A��A��AJA��AhsAĜA�A
n�A	�Av�A�;AO�AZA&�A��A=qAAM�AA �/A �@�5?@��@��!@�x�@��!@���@�@�+@�J@��T@�@�G�@�D@��@�j@���@�Q�@��@�O�@ۥ�@�J@ّh@�X@��`@�K�@�p�@�r�@�9X@�o@�ȴ@�M�@̓u@��@�dZ@�M�@�7L@�A�@�t�@ƸR@Ɵ�@�v�@��@ļj@�9X@�  @þw@�S�@�o@��H@�~�@�x�@�&�@���@�(�@���@�J@��-@���@��m@���@��@�=q@���@��@�Z@�ƨ@�t�@��@��R@���@���@���@���@���@���@�V@���@�p�@�%@�A�@�1'@�  @��@���@��@�9X@���@�C�@�v�@�?}@��@��@�Q�@�Q�@�I�@�I�@�I�@�ƨ@���@�V@�{@��@��T@���@��@�&�@���@�9X@�\)@�@�-@��-@�p�@�hs@�=q@��\@���@���@��u@�j@�Q�@���@��@��F@�t�@��H@�^5@���@�G�@�p�@��@��7@��7@�O�@�bN@��u@�Q�@�ƨ@�|�@�C�@�
=@���@�^5@��T@��7@��9@��@�l�@�l�@�|�@��@��@�t�@�K�@�o@���@���@���@���@��+@�~�@���@�ff@��@��h@���@���@�-@�E�@�/@�V@�%@�j@�(�@���@���@�|�@�|�@�t�@�C�@�33@��@��H@��@�@��H@��!@���@�ȴ@��R@�v�@��^@���@�?}@�V@��D@�(�@���@���@�l�@��
@�  @�9X@�I�@�z�@��@��@�Q�@� �@���@��
@���@�t�@�K�@���@��!@���@���@�V@��@��@���@��@�p�@�`B@�%@��`@��j@�z�@�Z@�9X@�(�@�1@��w@�\)@�33@��@��@���@�=q@���@���@�hs@�V@��@���@���@�r�@�A�@��m@��@�|�@�K�@�o@���@��@���@�v�@�M�@�J@��#@���@��^@��@�7L@�V@��`@�Ĝ@�r�@��@���@�K�@�;d@�;d@�@���@���@�n�@�^5@�-@���@��h@�/@���@��/@���@��D@�z�@��@��@\)@~�@~�+@~V@}�@}/@|�/@|z�@|�@{ƨ@{��@{�@{t�@z�@z�@y�@y��@yX@x��@x1'@w�;@w�@v��@vE�@up�@u�@s�
@sdZ@so@r��@rM�@q��@qx�@q�@p�`@q%@p��@pĜ@pA�@pb@o�@o�;@o�@n��@n��@n5?@m�-@mp�@m`B@l�j@lI�@k��@j�H@j=q@i�@i�7@iX@i�@hĜ@h�u@h�@hbN@hQ�@h1'@hb@h  @g�@g�;@g�@f�+@f$�@e��@e�h@e?}@e/@d��@d��@d��@d(�@ct�@c33@c@b�@b�H@b�!@bM�@bJ@a��@ahs@a%@`�9@`1'@`  @_��@_l�@^ȴ@^E�@]�@]��@]�@]�@\��@\z�@\j@\I�@[o@Z^5@Y��@Yx�@YG�@Y%@X �@W�P@W|�@W\)@W+@V�R@Vff@V@U�-@T�@T(�@Sƨ@S��@SC�@R�@R��@R=q@Q��@Q��@QX@Q�@P�`@P�9@PQ�@O�P@O|�@O+@O
=@Nȴ@N��@Nv�@M�-@M�@M`B@M/@M/@M/@MV@L�D@L1@K�
@K��@K��@K�@KdZ@KC�@K@J�@J��@J�\@J^5@J-@JJ@I�#@I�^@Ix�@H�`@H1'@G�@G�P@GK�@F�@FV@F5?@F@E�@D�j@D��@Dz�@DI�@Cƨ@CS�@Co@BM�@Ahs@@�9@@Q�@@A�@@1'@@  @?��@?l�@?\)@?�@>�R@>E�@>@=��@=�-@=p�@<��@<Z@<�@;��@;dZ@;C�@;@:��@:��@:��@:��@:�!@:^5@9�@9�7@9X@9G�@9G�@97L@8��@8��@8r�@8A�@8b@7�;@7�w@7�P@7|�@7K�@6�@6��@6v�@65?@6@5��@5�h@5O�@5/@4�@4��@4��@4�D@49X@3�
@3��@3t�@3C�@333@3"�@2�@2�!@2�\@2^5@2J@1�#@1G�@0��@0Ĝ@0�9@0�u@0r�@0Q�@0A�@/�@/�P@/;d@.�y@.��@.ff@.V@.@-�-@-�@-`B@-`B@-O�@-/@,��@,��@,��@,Z@+��@+�@+dZ@+S�@*�!@*M�@)��@)�^@)��@)X@)&�@(A�@(  @'�@'K�@&��@&5?@%O�@$�@$�j@$�D@$j@$I�@$9X@$1@#��@#C�@#@"�H@"��@"�!@"�\@"n�@"�@"J@!�#@!��@!��@!��@!��@!hs@!&�@ �`@ �9@ ��@ �u@ 1'@��@�@l�@+@��@�y@+@��@��@��@��@v�@E�@E�@{@@�@�h@/@(�@1@��@�m@ƨ@dZ@"�@�@��@��@��@��@@33@�!@�\@��@��@J@J@�^@��@�7@hs@7L@��@Ĝ@�@A�@�@��@��@��@��@;d@�y@�@ȴ@��@v�@E�@5?@{@@�@��@��@p�@/@V@��@�/@�D@j@�@1@�m@�F@t�@C�@�@��@��@��@��@^5@�@�7@X@G�@7L@7L@��@Ĝ@bN@Q�@Q�@A�@A�@1'@A�@  @�@�@�;@�;@�;@�;@�w@�P@\)@
=@��@@��@@�-@�@O�@V@�@�j@��@z�@Z@(�@�
@S�@o@
�!@
�!@
n�@
M�@
M�@
=q@
-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��AϮA�M�A���A���AήA΍PA�p�A�ZA�O�A�K�A�A�A�;dA�9XA�9XA�;dA�9XA�9XA�9XA�9XA�9XA�;dA�7LA�33A�/A�
=A�{A��A�K�A�;dA��A�t�A�VA��A�
=A�z�A��A���A�K�A���A�|�A��HA���A� �A���A�1'A��mA�/A��;A���A���A�{A��A�K�A�VA�v�A��FA��
A��A��uA�~�A���A�7LA��!A��TA�=qA��A��TA���A�;dA�A��A��wA�I�A��7A��A�?}A���A� �A�%A���A���A�n�A��A��`A��#A��DA���A�Q�A���A��A�n�A��TA��A�1'A���A�l�A���A��A��HA���A�oA�n�A���A��A�  A�Q�A}XAz�HAy33Av-AtȴArZAo;dAm��Am&�Al�AlȴAk�#AjAi|�Ah��Ah��Ah1'AeO�Ac�AbM�A_x�A^1A]�A\bNA[�7AZ��AYx�AW�;AW\)AUAT�AR�uAQ�7AOAK�AI��AG�;AE��AE�AEt�AE;dAD5?AB1A?�;A>VA> �A=�A=�^A=�^A=�A;�A:ZA9XA8�RA7��A6��A5��A5+A4~�A2��A1��A0�+A/;dA-��A-\)A,�A*jA)��A(=qA%oA"��A!\)A �HA��A�A�!A~�AdZA-A�#A�A��A�FA�A��AE�A�mA`BA  A��A��A��Az�A=qA�#A��A��AJA��AhsAĜA�A
n�A	�Av�A�;AO�AZA&�A��A=qAAM�AA �/A �@�5?@��@��!@�x�@��!@���@�@�+@�J@��T@�@�G�@�D@��@�j@���@�Q�@��@�O�@ۥ�@�J@ّh@�X@��`@�K�@�p�@�r�@�9X@�o@�ȴ@�M�@̓u@��@�dZ@�M�@�7L@�A�@�t�@ƸR@Ɵ�@�v�@��@ļj@�9X@�  @þw@�S�@�o@��H@�~�@�x�@�&�@���@�(�@���@�J@��-@���@��m@���@��@�=q@���@��@�Z@�ƨ@�t�@��@��R@���@���@���@���@���@���@�V@���@�p�@�%@�A�@�1'@�  @��@���@��@�9X@���@�C�@�v�@�?}@��@��@�Q�@�Q�@�I�@�I�@�I�@�ƨ@���@�V@�{@��@��T@���@��@�&�@���@�9X@�\)@�@�-@��-@�p�@�hs@�=q@��\@���@���@��u@�j@�Q�@���@��@��F@�t�@��H@�^5@���@�G�@�p�@��@��7@��7@�O�@�bN@��u@�Q�@�ƨ@�|�@�C�@�
=@���@�^5@��T@��7@��9@��@�l�@�l�@�|�@��@��@�t�@�K�@�o@���@���@���@���@��+@�~�@���@�ff@��@��h@���@���@�-@�E�@�/@�V@�%@�j@�(�@���@���@�|�@�|�@�t�@�C�@�33@��@��H@��@�@��H@��!@���@�ȴ@��R@�v�@��^@���@�?}@�V@��D@�(�@���@���@�l�@��
@�  @�9X@�I�@�z�@��@��@�Q�@� �@���@��
@���@�t�@�K�@���@��!@���@���@�V@��@��@���@��@�p�@�`B@�%@��`@��j@�z�@�Z@�9X@�(�@�1@��w@�\)@�33@��@��@���@�=q@���@���@�hs@�V@��@���@���@�r�@�A�@��m@��@�|�@�K�@�o@���@��@���@�v�@�M�@�J@��#@���@��^@��@�7L@�V@��`@�Ĝ@�r�@��@���@�K�@�;d@�;d@�@���@���@�n�@�^5@�-@���@��h@�/@���@��/@���@��D@�z�@��@��@\)@~�@~�+@~V@}�@}/@|�/@|z�@|�@{ƨ@{��@{�@{t�@z�@z�@y�@y��@yX@x��@x1'@w�;@w�@v��@vE�@up�@u�@s�
@sdZ@so@r��@rM�@q��@qx�@q�@p�`@q%@p��@pĜ@pA�@pb@o�@o�;@o�@n��@n��@n5?@m�-@mp�@m`B@l�j@lI�@k��@j�H@j=q@i�@i�7@iX@i�@hĜ@h�u@h�@hbN@hQ�@h1'@hb@h  @g�@g�;@g�@f�+@f$�@e��@e�h@e?}@e/@d��@d��@d��@d(�@ct�@c33@c@b�@b�H@b�!@bM�@bJ@a��@ahs@a%@`�9@`1'@`  @_��@_l�@^ȴ@^E�@]�@]��@]�@]�@\��@\z�@\j@\I�@[o@Z^5@Y��@Yx�@YG�@Y%@X �@W�P@W|�@W\)@W+@V�R@Vff@V@U�-@T�@T(�@Sƨ@S��@SC�@R�@R��@R=q@Q��@Q��@QX@Q�@P�`@P�9@PQ�@O�P@O|�@O+@O
=@Nȴ@N��@Nv�@M�-@M�@M`B@M/@M/@M/@MV@L�D@L1@K�
@K��@K��@K�@KdZ@KC�@K@J�@J��@J�\@J^5@J-@JJ@I�#@I�^@Ix�@H�`@H1'@G�@G�P@GK�@F�@FV@F5?@F@E�@D�j@D��@Dz�@DI�@Cƨ@CS�@Co@BM�@Ahs@@�9@@Q�@@A�@@1'@@  @?��@?l�@?\)@?�@>�R@>E�@>@=��@=�-@=p�@<��@<Z@<�@;��@;dZ@;C�@;@:��@:��@:��@:��@:�!@:^5@9�@9�7@9X@9G�@9G�@97L@8��@8��@8r�@8A�@8b@7�;@7�w@7�P@7|�@7K�@6�@6��@6v�@65?@6@5��@5�h@5O�@5/@4�@4��@4��@4�D@49X@3�
@3��@3t�@3C�@333@3"�@2�@2�!@2�\@2^5@2J@1�#@1G�@0��@0Ĝ@0�9@0�u@0r�@0Q�@0A�@/�@/�P@/;d@.�y@.��@.ff@.V@.@-�-@-�@-`B@-`B@-O�@-/@,��@,��@,��@,Z@+��@+�@+dZ@+S�@*�!@*M�@)��@)�^@)��@)X@)&�@(A�@(  @'�@'K�@&��@&5?@%O�@$�@$�j@$�D@$j@$I�@$9X@$1@#��@#C�@#@"�H@"��@"�!@"�\@"n�@"�@"J@!�#@!��@!��@!��@!��@!hs@!&�@ �`@ �9@ ��@ �u@ 1'@��@�@l�@+@��@�y@+@��@��@��@��@v�@E�@E�@{@@�@�h@/@(�@1@��@�m@ƨ@dZ@"�@�@��@��@��@��@@33@�!@�\@��@��@J@J@�^@��@�7@hs@7L@��@Ĝ@�@A�@�@��@��@��@��@;d@�y@�@ȴ@��@v�@E�@5?@{@@�@��@��@p�@/@V@��@�/@�D@j@�@1@�m@�F@t�@C�@�@��@��@��@��@^5@�@�7@X@G�@7L@7L@��@Ĝ@bN@Q�@Q�@A�@A�@1'@A�@  @�@�@�;@�;@�;@�;@�w@�P@\)@
=@��@@��@@�-@�@O�@V@�@�j@��@z�@Z@(�@�
@S�@o@
�!@
�!@
n�@
M�@
M�@
=q@
-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B&�B.B49B=qBE�BL�BR�BQ�BS�BS�BW
BW
BYB[#BYBZBffBhsBjBn�Bt�By�Bx�BgmBXBT�BYBR�BF�BB�BT�B`BBm�B~�B�+B�B�B~�Bz�Bs�Bp�Bn�Bm�Bn�BhsBffB^5BQ�BK�BB�B-B�BuBPB�B��B�}B�!B�PBz�Bv�BjBffBaHBR�B6FB/B.B)�B#�B%B
�B
�sB
�;B
��B
ɺB
�!B
��B
�B
_;B
E�B
;dB
%�B
�B
	7B	�B	�TB	�;B	�/B	�#B	�B	ǮB	B	�}B	�XB	�FB	��B	�oB	�VB	{�B	p�B	jB	ffB	`BB	[#B	VB	L�B	H�B	E�B	?}B	7LB	,B	$�B	1B��B�B�HB�BB�;B�5B�B��BƨB�qB�jB�dB�^B�XB�XB�9B�!B�'B�9B�FB�3B�'B�B�B��B��B��B��B��B�oB�VB�B|�Bt�BiyBW
BK�BG�BD�B<jB9XB7LB7LB33B1'B2-B0!B/B-B-B,B+B)�B,B)�B)�B(�B(�B)�B)�B(�B.B-B.B/B0!B/B/B-B/B-B.B.B1'B0!B/B1'B1'B1'B0!B2-B1'B0!B/B.B-B-B,B+B)�B)�B'�B&�B%�B%�B$�B&�B&�B'�B%�B&�B%�B&�B%�B%�B'�B(�B)�B1'B1'B0!B1'B5?B5?B6FB8RB;dB<jB>wBA�BA�BB�BC�BI�BI�BI�BJ�BJ�BK�BK�BL�BL�BK�BL�BL�BO�BO�BP�BR�BS�BS�BVBW
BYBZB\)B]/B]/B_;B_;BcTBe`BjBk�Bk�Bm�Bn�Bp�Bt�Bx�Bz�By�By�B~�B� B�B�B�1B�7B�PB�uB��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�'B�-B�dB��BÖBŢBƨBƨBǮBȴB��B�/B�HB�`B�B�B��B��B��B��B	B	B	%B	+B	
=B	DB	DB	JB	JB	PB	uB	�B	�B	�B	�B	�B	�B	#�B	'�B	,B	0!B	2-B	49B	6FB	;dB	?}B	?}B	@�B	B�B	E�B	I�B	K�B	K�B	L�B	L�B	M�B	O�B	T�B	YB	XB	ZB	]/B	aHB	cTB	gmB	k�B	l�B	m�B	n�B	n�B	o�B	t�B	x�B	x�B	x�B	x�B	z�B	�B	�B	�B	�+B	�1B	�1B	�=B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�?B	�?B	�?B	�?B	�LB	�RB	�RB	�^B	�dB	�jB	�qB	�wB	�wB	��B	��B	B	B	ÖB	ÖB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B
1B
1B
1B
1B

=B
DB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
#�B
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
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
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
;dB
:^B
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
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
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
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
Q�B
P�B
P�B
P�B
P�B
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
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
YB
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
[#B
[#B
\)B
]/B
]/B
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
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
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
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
iyB
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
l�B
l�B
l�B
l�B
l�B
l�B
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
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B~BxBxBqBqBqBkBkBkBkBkBkBkBkBkBqBxBxBxB~B~B�B�B�B~BYBqB_B&�B-�B4B=<BEmBL�BR�BQ�BS�BS�BV�BV�BX�BZ�BX�BY�Bf2Bh>BjKBncBt�By�Bx�Bg8BW�BT�BX�BR�BFtBBABT�B`Bm]B~�B��B��B��B~�Bz�Bs�BpoBncBm]BncBh>BfB]�BQ�BK�BB[B,�B9B@BB�UBҽB�.B��B�Bz�BvzBjKBfBaBR�B6B.�B-�B)�B#�B�B
�B
�>B
��B
өB
�lB
��B
�kB
��B
^�B
ESB
;0B
%�B
YB
�B	�hB	�B	�B	��B	��B	��B	�zB	�[B	�.B	�	B	�B	��B	� B	�B	{�B	pUB	jKB	fB	`B	Z�B	U�B	L~B	HfB	ESB	?.B	7B	+�B	$�B	�B��B�aB��B�B��B��B��BΥB�YB�"B�B�B�*B�	B�	B��B��B��B��B�B��B��B��B��B��B��B�~B�WB�SB� B�B��B|�BtnBi*BV�BKxBG_BDMB<B9	B6�B6�B2�B0�B1�B/�B.�B,�B,�B+�B*�B)�B+�B)�B)�B(�B(�B)�B)�B(�B-�B,�B-�B.�B/�B.�B.�B,�B.�B,�B-�B-�B0�B/�B.�B0�B0�B0�B/�B1�B0�B/�B.�B-�B,�B,�B+�B*�B)�B)�B'�B&�B%�B%�B$�B&�B&�B'�B%�B&�B%�B&�B%�B%�B'�B(�B)�B0�B0�B/�B0�B4�B4�B5�B8B;B<B>(BA;BA BB'BCGBIlBIRBIlBJrBJrBK^BK^BLdBL~BKxBL~BL~BO�BO�BP�BR�BS�BS�BU�BV�BX�BY�B[�B\�B\�B^�B^�BcBeBj0Bk6BkBmCBnIBpUBtnBx�BzxByrBy�B~�B�B��B��B��B��B��B�&B�9B�EB�QB�QB�7B�QB�KB�vB��B��B��B��B��B��B��B��B��B�4B�GB�9B�YB�YB�_B�fBЗB��B��B�B�CB�[B�nB��B��B��B	 �B	�B	�B	�B		�B	
�B	
�B	�B	�B	�B	&B	EB	QB	WB	]B	dB	pB	#�B	'�B	+�B	/�B	1�B	3�B	5�B	;B	?.B	?.B	@4B	BAB	E9B	IlB	K^B	K^B	L~B	L~B	M�B	O�B	T�B	X�B	W�B	Y�B	\�B	`�B	b�B	gB	k6B	l"B	mCB	n/B	nIB	oOB	tnB	x�B	xlB	x�B	x�B	z�B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�9B	�9B	�$B	�?B	�?B	�?B	�+B	�1B	�1B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�"B	�(B	�(B	�4B	�;B	�AB	�AB	�-B	�GB	�9B	�YB	�?B	�_B	�_B	�fB	�RB	�dB	�jB	�pB	ϑB	ϑB	�vB	�}B	ЗB	ЗB	ѝB	ңB	өB	ԯB	յB	ּB	��B	רB	خB	��B	��B	��B	ںB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�$B	�$B	�
B	�B	�6B	�=B	�CB	�)B	�)B	�/B	�/B	�IB	�UB	�UB	�[B	�GB	�aB	�aB	�hB	�nB	�nB	�tB	�ZB	�zB	�zB	�zB	�zB	��B	��B	�rB	��B	��B	��B	�xB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
�B
B
�B
�B
B
�B
�B
B
B
B
B
�B
B
B
�B
�B
�B
B
�B
B
�B
B
B
B
 B
&B
&B
&B
&B
,B
,B
,B
2B
2B
B
9B
9B
$B
$B
$B
$B
+B
KB
KB
1B
QB
QB
WB
=B
WB
=B
=B
dB
jB
pB
pB
pB
VB
 vB
!|B
!|B
!|B
!|B
"hB
"�B
#�B
#�B
$tB
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
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
0�B
0�B
0�B
1�B
1�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
4�B
5�B
5�B
5�B
6�B
8B
8B
7�B
8B
8B
9	B
9	B
8�B
9	B
8�B
:B
:B
:B
:B
:�B
:B
;B
:�B
:�B
<B
<B
<B
<B
<B
<B
<B
="B
=B
>(B
?.B
?.B
?B
?.B
?.B
@4B
@4B
@4B
A;B
A;B
A;B
A B
A B
A;B
BAB
BAB
B'B
BAB
B'B
CGB
CGB
CGB
C-B
CGB
DMB
D3B
D3B
DMB
D3B
DMB
E9B
ESB
ESB
ESB
ESB
ESB
ESB
ESB
FYB
F?B
F?B
G_B
G_B
G_B
G_B
G_B
HfB
HfB
HKB
HfB
IlB
IRB
IlB
JXB
JrB
JrB
JrB
K^B
K^B
KxB
K^B
KxB
KxB
KxB
L~B
L~B
L~B
L~B
M�B
M�B
MjB
M�B
N�B
O�B
O�B
O�B
OvB
OvB
Q�B
P�B
P�B
P�B
P}B
O�B
OvB
O�B
O�B
O�B
P�B
P�B
P}B
P}B
P�B
P�B
P}B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
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
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z�B
Z�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
cB
cB
cB
cB
b�B
cB
cB
dB
dB
c�B
dB
dB
c�B
dB
dB
dB
dB
d�B
eB
eB
d�B
eB
d�B
d�B
eB
eB
eB
fB
fB
gB
gB
gB
gB
h$B
h$B
h$B
iB
j0B
j0B
jB
jB
jB
iB
j0B
j0B
j0B
j0B
jB
jB
jB
k6B
k6B
k6B
l=B
l=B
l=B
l=B
l=B
l"B
l=B
l=B
l"B
mCB
mCB
mCB
mCB
nIB
nIB
nIB
nIB
nI11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.64(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809240042132018092400421320180924004213201809250043272018092500432720180925004327JA  ARFMdecpA19c                                                                20180919123513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180919033530  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180919033531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180919033531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180919033532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180919033532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180919033532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180919033532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180919033532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180919033532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180919035522                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180919154002  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180923154213  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180923154213  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180924154327  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                