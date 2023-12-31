CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-19T21:37:15Z creation;2019-12-19T21:37:19Z conversion to V3.1;2023-06-29T05:50:26Z update;     
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
resolution        =���   axis      Z        X  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  `d   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۼ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191219213715  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_197                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�����S�1   @���u� @7����+�b�
=p��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @S�
@�Q�@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*h�C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	�
D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRDRD��RD�RD�PRDÐRD��RD�RD�PRDĐRD��RD�RD�PRDŐRD��RD�RD�PRDƐRD��RD�RD�PRDǐRD��RD�RD�PRDȐRD��RD�RD�PRDɐRD��RD�RD�PRDʐRD��RD�RD�PRDːRD��RD�RD�PRD̐RD��RD�RD�PRD͐RD��RD�RD�PRDΐRD��RD�RD�PRDϐRD��RD�RD�PRDАRD��RD�RD�PRDѐRD��RD�RD�PRDҐRD��RD�RD�PRDӐRD��RD�RD�PRDԐRD��RD�RD�PRDՐRD��RD�RD�PRD֐RD��RD�RD�PRDאRD��RD�RD�PRDؐRD��RD�RD�PRDِRD��RD�RD�PRDڐRD��RD�RD�PRDېRD��RD�RD�PRDܐRD��RD�RD�PRDݐRD��RD�RD�PRDސRD��RD�RD�PRDߐRD��RD�RD�PRD��RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD��D�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD��RD��RD�RD�PRD��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�1'A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�;dA�;dA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�C�A�C�A�E�A�G�A�G�A�I�A�G�A�"�A̲-A��
A˕�A�hsA�O�A�=qA���A��/A���A�ĜAʍPA�9XA�l�A�1A�ZA�/A�{A��A���A��A�{A�t�A�E�A��jA�5?A�+A�v�A�%A��A��A�1A�=qA�~�A�ZA���A�^5A��A�{A���A�K�A�r�A�(�A�S�A��+A��mA�dZA��A��hA��^A��!A�7LA�5?A��A�O�A�r�A���A�Q�A��A�$�A��A���A��+A��hA��^A��+A�VA���A�1A�n�A�v�A�A�hsA�bNA��A��/A�?}A�x�A�=qA�mA���A�7LA�5?A~�Az��Ay�Aw��AvffAs%Ap{Anz�Am��Am/Aj��AiG�Ai�AhbAe�7AbE�A_��A]`BAZ��AX��AU��AT5?AR�!AQ��AP�uANbAK�#AJ�AH��AEK�AC��AA�hA@ĜA@�A>ĜA<bA:VA8��A7�;A6��A5�A4�!A3��A3
=A1��A/�mA-�A+��A*ĜA)ƨA(��A(E�A'�hA&�A&�A&�A&v�A&z�A#A �DA�A��A33A
=A�+A��AjA�AoA�TAS�A=qA��A��Al�A�A��A\)AbNA�^AS�A=qA�7A�jAJA�`A�AXA
�RA
=qA
$�A	�A	�AVA��Ax�A\)A�A�9A�+A�-A33A��A�A|�AjA�mA��A7L@�;d@�%@�ȴ@��-@�"�@���@�l�@�Ĝ@�M�@�-@�bN@�l�@�~�@�G�@�o@�-@��@�9X@��H@��u@��m@ݡ�@���@�I�@��m@ە�@�33@�ȴ@���@ڗ�@��@��#@�7L@�bN@��y@�7L@��@϶F@�33@�`B@ȣ�@��`@ư!@þw@�C�@°!@¸R@�^5@��@�C�@��`@��@�{@Å@�`B@���@��@�"�@� �@�hs@��`@�G�@�"�@�$�@��@��j@��/@�9X@��+@�hs@�bN@�t�@�J@�O�@�?}@��7@��D@�33@��@�5?@�/@��
@�E�@���@���@��D@�1@�+@�+@�bN@�%@�j@��w@�\)@�M�@��@��@���@�r�@��@��;@���@�&�@�"�@�b@� �@�1@��m@��m@�t�@��\@��@��!@��#@�ff@�K�@��@��u@��9@���@�V@���@��@��F@�C�@�K�@�|�@�r�@�J@��+@�$�@�^5@��@�j@�I�@�A�@�I�@�Q�@�(�@��@�"�@�j@���@��j@�/@��@��#@���@�r�@���@�@�@���@�%@��/@���@�K�@��@���@�z�@�Q�@��j@�C�@���@��@�?}@�`B@���@��@���@��u@���@�S�@�dZ@�K�@�M�@�G�@�~�@�n�@�=q@�p�@��@���@�@�;d@�j@�I�@�1'@� �@�ƨ@�C�@�$�@�&�@�G�@��`@�(�@�
=@�E�@�J@�hs@��@�K�@��@�M�@�{@�@���@��h@���@�hs@�O�@�7L@��@��9@�Z@�A�@�(�@�  @��@�t�@�l�@���@��m@���@��P@���@���@��#@��^@�`B@�7L@�V@�%@���@��@��9@��D@�bN@�Z@�A�@� �@��@�  @��;@��F@���@�l�@�\)@�C�@��@�
=@���@��y@��@��R@��\@�^5@�$�@��@��-@�O�@�7L@��@�V@���@��@��D@�j@�9X@��F@�dZ@�C�@�33@���@���@���@�~�@�v�@�M�@�J@��^@��h@�x�@�hs@�O�@�?}@�%@�Ĝ@��@���@�Z@�A�@�(�@�;@|�@�@~��@~E�@}�T@}�h@}V@|�/@|�D@|9X@|1@{�
@{t�@{C�@{"�@z�!@zn�@z-@y��@y��@yhs@yG�@y�@xbN@x  @w�;@w|�@w�@v�R@v5?@u�@u��@u�@t�@t��@tZ@s�F@s�@sS�@s"�@so@s@r��@rn�@q��@q&�@p��@pbN@o�@o|�@o;d@o
=@n��@n$�@m�T@m��@m�@l��@lz�@l9X@k�@j�H@j�\@jn�@j^5@i��@ix�@ix�@i&�@hQ�@hA�@g�;@g;d@fȴ@fff@f@e��@ep�@e?}@eV@d��@dz�@d1@c�
@c��@cS�@co@b��@b�\@bM�@a�@a�7@aG�@a%@`��@`A�@`  @_��@_|�@^�y@^�R@^E�@]�@]�T@]�T@]��@]�@\��@\(�@[ƨ@[�@[S�@["�@Z�H@Z��@Z^5@ZJ@Y��@Y7L@Y%@X��@X�9@Xb@W�@W\)@W
=@V�@Vȴ@V��@V@Up�@U�@T��@T�j@TI�@Sƨ@S��@S�@S"�@R�!@R-@Q��@Qx�@Q7L@P�`@P��@PA�@O�@O�P@N��@N�@N�R@N��@NV@N5?@N$�@N$�@M�-@MV@L�@K��@K��@K33@J��@J��@J=q@I��@I�@I��@I��@I7L@I%@HĜ@H��@G�@G|�@GK�@F�y@F�R@Fff@E�-@E/@EV@D�/@Dz�@D9X@Cƨ@C�F@C��@C33@B��@B=q@B-@A��@A��@A�^@A��@Ahs@A7L@@��@@�9@@�9@@��@@  @?|�@?;d@>ȴ@=�@=��@=�@=p�@=V@<I�@<1@;�m@;�m@;��@;C�@;33@;33@:n�@9��@9��@9��@9��@9G�@8�9@8Q�@8 �@7�;@6�y@6�R@6v�@65?@5��@5�@5`B@5`B@4��@4�D@4(�@3�
@3ƨ@3��@333@3@2�H@2��@2~�@2M�@2=q@1��@17L@1�@0��@0r�@0b@/�@/|�@/;d@.�@.��@.�+@.E�@.{@-�@-�T@-�h@-O�@-�@-V@,��@,�/@,�D@,j@,I�@+��@+�F@+�@+dZ@+33@+@*�H@*��@*��@*��@*�!@*n�@*M�@*=q@*-@*-@*�@*J@*J@)��@)�#@)�^@)��@)��@)��@)x�@)G�@)&�@)&�@)&�@)%@(Ĝ@(A�@(1'@(b@'�;@'�P@'K�@'+@'+@'�@&��@&ȴ@&ff@&@%�@%@%�-@%O�@$�/@$z�@$I�@$(�@$1@#��@#�m@#ƨ@#�@#dZ@#S�@#"�@#o@"�@"��@"~�@"^5@"-@!�@!�^@!��@!x�@!G�@!&�@!&�@!�@ Ĝ@ �@ bN@  �@�@�@�P@l�@;d@�@ȴ@E�@$�@@�@��@��@p�@`B@`B@O�@�@��@��@j@Z@�@��@�m@�F@�@S�@C�@o@�H@��@��@M�@J@�^@x�@G�@G�@7L@�@%@�`@�@1'@�@��@;d@+@+@
=@�R@��@V@@�-@�@�@`B@V@�@9X@�@�m@�m@�m@�m@�
@�@C�@"�@�@��@��@�\@n�@�@�#@�^@�7@X@X@G�@&�@�@&�@�@%@��@��@�9@��@��@�u@Q�@ �@b@b@�@�w@�P@l�@;d@�@�y@��@V@@�@�@@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�1'A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�;dA�;dA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�C�A�C�A�E�A�G�A�G�A�I�A�G�A�"�A̲-A��
A˕�A�hsA�O�A�=qA���A��/A���A�ĜAʍPA�9XA�l�A�1A�ZA�/A�{A��A���A��A�{A�t�A�E�A��jA�5?A�+A�v�A�%A��A��A�1A�=qA�~�A�ZA���A�^5A��A�{A���A�K�A�r�A�(�A�S�A��+A��mA�dZA��A��hA��^A��!A�7LA�5?A��A�O�A�r�A���A�Q�A��A�$�A��A���A��+A��hA��^A��+A�VA���A�1A�n�A�v�A�A�hsA�bNA��A��/A�?}A�x�A�=qA�mA���A�7LA�5?A~�Az��Ay�Aw��AvffAs%Ap{Anz�Am��Am/Aj��AiG�Ai�AhbAe�7AbE�A_��A]`BAZ��AX��AU��AT5?AR�!AQ��AP�uANbAK�#AJ�AH��AEK�AC��AA�hA@ĜA@�A>ĜA<bA:VA8��A7�;A6��A5�A4�!A3��A3
=A1��A/�mA-�A+��A*ĜA)ƨA(��A(E�A'�hA&�A&�A&�A&v�A&z�A#A �DA�A��A33A
=A�+A��AjA�AoA�TAS�A=qA��A��Al�A�A��A\)AbNA�^AS�A=qA�7A�jAJA�`A�AXA
�RA
=qA
$�A	�A	�AVA��Ax�A\)A�A�9A�+A�-A33A��A�A|�AjA�mA��A7L@�;d@�%@�ȴ@��-@�"�@���@�l�@�Ĝ@�M�@�-@�bN@�l�@�~�@�G�@�o@�-@��@�9X@��H@��u@��m@ݡ�@���@�I�@��m@ە�@�33@�ȴ@���@ڗ�@��@��#@�7L@�bN@��y@�7L@��@϶F@�33@�`B@ȣ�@��`@ư!@þw@�C�@°!@¸R@�^5@��@�C�@��`@��@�{@Å@�`B@���@��@�"�@� �@�hs@��`@�G�@�"�@�$�@��@��j@��/@�9X@��+@�hs@�bN@�t�@�J@�O�@�?}@��7@��D@�33@��@�5?@�/@��
@�E�@���@���@��D@�1@�+@�+@�bN@�%@�j@��w@�\)@�M�@��@��@���@�r�@��@��;@���@�&�@�"�@�b@� �@�1@��m@��m@�t�@��\@��@��!@��#@�ff@�K�@��@��u@��9@���@�V@���@��@��F@�C�@�K�@�|�@�r�@�J@��+@�$�@�^5@��@�j@�I�@�A�@�I�@�Q�@�(�@��@�"�@�j@���@��j@�/@��@��#@���@�r�@���@�@�@���@�%@��/@���@�K�@��@���@�z�@�Q�@��j@�C�@���@��@�?}@�`B@���@��@���@��u@���@�S�@�dZ@�K�@�M�@�G�@�~�@�n�@�=q@�p�@��@���@�@�;d@�j@�I�@�1'@� �@�ƨ@�C�@�$�@�&�@�G�@��`@�(�@�
=@�E�@�J@�hs@��@�K�@��@�M�@�{@�@���@��h@���@�hs@�O�@�7L@��@��9@�Z@�A�@�(�@�  @��@�t�@�l�@���@��m@���@��P@���@���@��#@��^@�`B@�7L@�V@�%@���@��@��9@��D@�bN@�Z@�A�@� �@��@�  @��;@��F@���@�l�@�\)@�C�@��@�
=@���@��y@��@��R@��\@�^5@�$�@��@��-@�O�@�7L@��@�V@���@��@��D@�j@�9X@��F@�dZ@�C�@�33@���@���@���@�~�@�v�@�M�@�J@��^@��h@�x�@�hs@�O�@�?}@�%@�Ĝ@��@���@�Z@�A�@�(�@�;@|�@�@~��@~E�@}�T@}�h@}V@|�/@|�D@|9X@|1@{�
@{t�@{C�@{"�@z�!@zn�@z-@y��@y��@yhs@yG�@y�@xbN@x  @w�;@w|�@w�@v�R@v5?@u�@u��@u�@t�@t��@tZ@s�F@s�@sS�@s"�@so@s@r��@rn�@q��@q&�@p��@pbN@o�@o|�@o;d@o
=@n��@n$�@m�T@m��@m�@l��@lz�@l9X@k�@j�H@j�\@jn�@j^5@i��@ix�@ix�@i&�@hQ�@hA�@g�;@g;d@fȴ@fff@f@e��@ep�@e?}@eV@d��@dz�@d1@c�
@c��@cS�@co@b��@b�\@bM�@a�@a�7@aG�@a%@`��@`A�@`  @_��@_|�@^�y@^�R@^E�@]�@]�T@]�T@]��@]�@\��@\(�@[ƨ@[�@[S�@["�@Z�H@Z��@Z^5@ZJ@Y��@Y7L@Y%@X��@X�9@Xb@W�@W\)@W
=@V�@Vȴ@V��@V@Up�@U�@T��@T�j@TI�@Sƨ@S��@S�@S"�@R�!@R-@Q��@Qx�@Q7L@P�`@P��@PA�@O�@O�P@N��@N�@N�R@N��@NV@N5?@N$�@N$�@M�-@MV@L�@K��@K��@K33@J��@J��@J=q@I��@I�@I��@I��@I7L@I%@HĜ@H��@G�@G|�@GK�@F�y@F�R@Fff@E�-@E/@EV@D�/@Dz�@D9X@Cƨ@C�F@C��@C33@B��@B=q@B-@A��@A��@A�^@A��@Ahs@A7L@@��@@�9@@�9@@��@@  @?|�@?;d@>ȴ@=�@=��@=�@=p�@=V@<I�@<1@;�m@;�m@;��@;C�@;33@;33@:n�@9��@9��@9��@9��@9G�@8�9@8Q�@8 �@7�;@6�y@6�R@6v�@65?@5��@5�@5`B@5`B@4��@4�D@4(�@3�
@3ƨ@3��@333@3@2�H@2��@2~�@2M�@2=q@1��@17L@1�@0��@0r�@0b@/�@/|�@/;d@.�@.��@.�+@.E�@.{@-�@-�T@-�h@-O�@-�@-V@,��@,�/@,�D@,j@,I�@+��@+�F@+�@+dZ@+33@+@*�H@*��@*��@*��@*�!@*n�@*M�@*=q@*-@*-@*�@*J@*J@)��@)�#@)�^@)��@)��@)��@)x�@)G�@)&�@)&�@)&�@)%@(Ĝ@(A�@(1'@(b@'�;@'�P@'K�@'+@'+@'�@&��@&ȴ@&ff@&@%�@%@%�-@%O�@$�/@$z�@$I�@$(�@$1@#��@#�m@#ƨ@#�@#dZ@#S�@#"�@#o@"�@"��@"~�@"^5@"-@!�@!�^@!��@!x�@!G�@!&�@!&�@!�@ Ĝ@ �@ bN@  �@�@�@�P@l�@;d@�@ȴ@E�@$�@@�@��@��@p�@`B@`B@O�@�@��@��@j@Z@�@��@�m@�F@�@S�@C�@o@�H@��@��@M�@J@�^@x�@G�@G�@7L@�@%@�`@�@1'@�@��@;d@+@+@
=@�R@��@V@@�-@�@�@`B@V@�@9X@�@�m@�m@�m@�m@�
@�@C�@"�@�@��@��@�\@n�@�@�#@�^@�7@X@X@G�@&�@�@&�@�@%@��@��@�9@��@��@�u@Q�@ �@b@b@�@�w@�P@l�@;d@�@�y@��@V@@�@�@@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�dB
�dB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�dB
�dB
�dB
�dB
�}B
��B�B�B �B#�B&�B,B0!B49B7LB<jB<jB?}BM�Bx�B�jB�BB��B��B��BBDBPB�B'�B2-B)�B-B<jBL�BP�BXBYBaHBaHB^5BZBYBZBZBR�BP�BM�BM�BH�B?}B<jB6FB�B��B�;B��B�oB�B�PB�Bv�BZBS�BL�BH�BK�B?}B,BbB
��B
��B
�XB
jB
7LB
VB
uB
�B
�B
�B
�B	�yB	��B
B
H�B
?}B
6FB
�B

=B
B	��B	�ZB	ȴB	�dB	�3B	�'B	��B	��B	�oB	�JB	}�B	gmB	R�B	A�B	1'B	"�B	oB	1B��B��B�B�fB�B��BĜB�B��B��B��B��B��B�uB�7B�DB�1B�1B�B�B�B|�B{�Bv�Bs�Bk�BffBcTB^5B\)B[#B^5Be`BhsBv�B�B� Bk�Bp�B|�B�B�B� B|�Bu�Bo�Bv�Bw�Bu�Bq�Bn�Bm�Bk�BiyBe`B[#BXBXBW
BR�BQ�BO�BO�BP�BP�BP�BO�BO�BO�BQ�BT�BW
BYBYBZB[#B^5B^5B]/B]/BaHBhsBgmBgmBe`BdZBcTBdZBffB_;BiyBe`BffB`BB[#BT�BXBR�BQ�BM�BJ�BE�BB�B@�B?}B>wB:^B8RB49B2-B49B;dBC�BH�BM�BR�B]/BcTBiyBo�Bq�Bp�Bo�BhsB_;B\)B[#BQ�BVBYBG�BF�BI�BN�BQ�BVB]/Bk�Bl�B�B�B|�B�B�7B�%B�JB��B��B�=B�%B�B�B�B�1B�\B�VB�PB�DB�DB�1B�=B�hB��B��B��B��B��B��B��B��B��B��B��B�B�9B�qB��B�#B�NB�ZB�`B�`B�fB�B�B�B�B�B�B��B	PB	�B	�B	�B	�B	�B	�B	�B	\B		7B	%B	VB	�B	�B	2-B	7LB	5?B	49B	49B	;dB	E�B	E�B	E�B	G�B	L�B	\)B	`BB	`BB	T�B	O�B	N�B	O�B	P�B	Q�B	R�B	VB	W
B	ZB	dZB	t�B	s�B	w�B	�B	�B	� B	}�B	|�B	z�B	}�B	�B	�B	� B	�B	|�B	t�B	r�B	s�B	s�B	v�B	q�B	{�B	�B	�+B	�=B	�\B	�oB	�uB	�hB	�VB	�VB	�\B	�VB	�VB	�DB	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�dB	�dB	�jB	�jB	�jB	�dB	�XB	�dB	�dB	�XB	�?B	�-B	�3B	�-B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�?B	�?B	�FB	�RB	�^B	�dB	�dB	�dB	�jB	�jB	�qB	��B	ŢB	ŢB	ŢB	ŢB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
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
B
B
B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B
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
bB
hB
hB
hB
oB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
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
,B
-B
.B
.B
.B
/B
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
2-B
2-B
2-B
33B
33B
33B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
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
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
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
K�B
K�B
K�B
K�B
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
O�B
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
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
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
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
`BB
`BB
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
bNB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
hsB
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
k�B
k�B
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
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�JB
�B
�DB
�*B
�*B
�*B
�JB
�0B
�0B
�0B
�0B
�JB
�JB
�0B
�JB
�0B
�0B
�0B
�0B
�6B
�6B
�PB
�6B
�6B
�0B
�dB
��B
�PB
B
��B�B�B �B#�B'8B,"B0!B4TB7�B=�B>�BC�BTaB��B��B��B�B �B��B�}BMB�B.B �B)DB33B,B/iB>BBNVBR�BZ�B\xBd@Bc�B_�BZ�BZQB\B\�BT�BR�BOBBO(BJ�BA�B>wB8�B#TB[B��B�B�,B�GB��B�Bw�BZ�BTaBMBI7BM�BB'B.�B[B
��B
�eB
�HB
nB
:�B
�B
�B
B
EB
B
dB	�B	�"B
 �B
J#B
A�B
:*B
�B
0B
B	�rB	�8B	�rB	�jB	�TB	��B	�B	�9B	�,B	��B	��B	j�B	U�B	DgB	4B	%�B	aB		�B	 4B��B�hB��B��BӏB�1B�;B�
B��B��B�&B��B��B��B�dB��B��B��B�-B��B~�B~wBy>Bu�Bl�Bg�BdZB^�B\�B[�B^�Be,BhsBw�B��B�[BlWBqB}<B�[B��B� B~wBv�Bo�BxBxlBv�Bq�Bn�Bm�BlWBk6Bg�B\CBX�BX�BX+BS�BR�BP�BQBQ�BQ�BQ�BPHBPBP}BR�BU�BW�BY1BYBZkB[�B^�B_!B]�B]�Ba�BiDBh�Bg�Be�Be,Be,Be�BgmB`BBj�BffBg�Ba�B\]BUMBX�BS�BRoBN�BK�BF%BCGBAB@iB?�B:�B9rB4�B2|B4TB;dBC�BH�BM�BSB]dBc�Bi�Bp;Br�Bq�Bq�BiDB_�B]�B]~BRTBWsBZQBG�BF�BI�BN�BQhBUgB\]Bk�Bl"B�GB��B|�B��B��B�SB��B�B�_B�^B��B��B�3B��B��B�.B��B��B��B��B��B�#B�NB�B�1B�B��B�|B��B��B�2B�2B�
B�$B�]B��B��BуB�WB�B�tB��B�`B�B��B�B�qB�]B�)B�B��B	~B	2B	YB	mB	sB	�B	EB	�B	.B		RB	�B	�B	�B	MB	2B	8B	5ZB	4B	3hB	:�B	EmB	E9B	EB	F�B	K�B	[�B	`�B	b4B	U�B	PB	N�B	O�B	P�B	Q�B	R�B	VSB	V�B	Y1B	c�B	t�B	s3B	wLB	��B	�aB	�OB	~�B	}<B	z�B	~B	�'B	�B	�OB	��B	~B	t�B	r�B	s�B	s�B	wB	p�B	{dB	��B	��B	��B	�(B	��B	��B	��B	�<B	�<B	�\B	��B	�pB	��B	�2B	��B	��B	�mB	�B	�/B	��B	�QB	�*B	�0B	�JB	��B	��B	��B	��B	�>B	�B	��B	��B	�tB	�GB	��B	��B	�iB	�=B	�"B	�B	�B	�B	��B	�B	�B	�%B	�%B	�?B	�FB	�RB	�DB	�0B	�JB	�dB	�6B	�6B	�B	� B	ŢB	ŢB	��B	��B	ĜB	ňB	ƎB	ǔB	ȀB	ȚB	ɆB	ɠB	ɺB	ʦB	ˬB	ˬB	̘B	ΥB	��B	��B	��B	бB	��B	ѷB	ҽB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�B	�B	�B	�B	�B	�'B	�'B	�HB	�:B	�,B	�FB	�FB	�fB	�2B	�2B	�RB	�XB	�_B	�eB	�kB	�qB	�qB	�WB	�wB	�B	�B	�iB	�iB	�B	�oB	�oB	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
9B
B
B
B
B
�B
�B
B
B
�B
B
KB
	B
	B
	B

XB

#B
)B
0B
B
0B
JB
6B
6B
jB
"B
VB
<B
<B
\B
(B
BB
.B
HB
HB
4B
4B
hB
TB
:B
[B
[B
FB
aB
aB
MB
�B
mB
SB
mB
YB
sB
_B
yB
�B
eB
B
eB
QB
kB
B
B
�B
�B
qB
qB
�B
�B
�B
�B
~B
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
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
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
+�B
+�B
,�B
-�B
-�B
-�B
/ B
-�B
/ B
/ B
.�B
0B
0B
/�B
1B
1B
1'B
2-B
2B
2-B
3B
33B
33B
5%B
5%B
5B
5%B
6B
6FB
72B
7B
72B
7LB
8RB
9>B
9$B
9$B
9$B
9>B
9$B
:DB
:DB
:*B
:B
:DB
:DB
:^B
:*B
:DB
:^B
;JB
;0B
;0B
;dB
<jB
<PB
=<B
=<B
=<B
=VB
=<B
=<B
>�B
?cB
@OB
@4B
@iB
A�B
AoB
B[B
BuB
B�B
B�B
CaB
CaB
D�B
DgB
D�B
DgB
EmB
E�B
E�B
E�B
FtB
F�B
FtB
GzB
GzB
GzB
GzB
G�B
GzB
GzB
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
XB
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
Y�B
[	B
[	B
[	B
[	B
[	B
[	B
[�B
[�B
[�B
[�B
[�B
\B
[�B
[�B
[�B
]B
]B
]B
]B
\�B
^B
^B
_;B
_B
_!B
_B
_!B
`'B
`'B
`B
_�B
`B
`'B
`'B
`B
`B
`B
a-B
aB
a-B
aB
aB
bB
bB
b4B
b4B
b4B
b4B
b4B
c:B
c:B
d@B
d@B
d&B
d&B
d&B
d@B
d&B
d@B
d@B
d&B
eFB
e,B
e,B
eB
e,B
e,B
fLB
f2B
fLB
fLB
fLB
f2B
fLB
fLB
g8B
gRB
gRB
hXB
h>B
h>B
h>B
h>B
h>B
hXB
h>B
hXB
hXB
h>B
h>B
hXB
i_B
iDB
iDB
i_B
i_B
iDB
j0B
jKB
j0B
j0B
j0B
jKB
jeB
k6B
kQB
lWB
l=B
lWB
lqB
lqB
lWB
l=B
m]B
m]B
mwB
mwB
mwB
m]B
ncB
ncB
ncB
n}B
oiB
oiB
o�B
oiB
oiB
oi1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.51(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912250036142019122500361420191225003614202306231719392023062317193920230623171939201912260030322019122600303220191226003032  JA  ARFMdecpA19c                                                                20191220063713  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191219213715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191219213717  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191219213718  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191219213718  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191219213718  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191219213718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191219213718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191219213719  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191219213719                      G�O�G�O�G�O�                JA  ARUP                                                                        20191219215353                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191220153558  CV  JULD            G�O�G�O�Fǥm                JM  ARCAJMQC2.0                                                                 20191224153614  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191224153614  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191225153032  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081939  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                