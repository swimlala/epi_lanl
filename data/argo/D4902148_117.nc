CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-10-30T15:35:08Z creation;2017-10-30T15:35:11Z conversion to V3.1;2019-12-18T07:27:10Z update;2022-11-21T05:31:46Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20171030153508  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               uA   JA  I1_0397_117                     2C  Dd%NAVIS_A                         0397                            ARGO 011514                     863 @�1�ɓ� 1   @�1�����@;$��E��d%�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@:=q@�Q�@�Q�A (�A (�AAA`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D
D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJz=DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw
Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RDͽD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�F�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��`AăAć+AąAăAć+AċDAĉ7Ać+AăAāAć+Ać+AąAăA�|�A�z�A�|�A�|�A�v�A�=qA�ĜAç�A�t�A�I�A�+A��A�A�n�A�$�A��A��A��^A��`A��-A�=qA�=qA��HA�/A���A��yA�ZA��FA���A�A��A�hsA�&�A��A�t�A�I�A��A�VA��/A�
=A�1A��A��HA���A�hsA�"�A���A�%A�;dA��;A��TA�?}A�bA�&�A���A�1A���A��;A�bA�$�A�Q�A�A/A}�Az(�Ay��Ayp�AyG�Ay�Ax��Ax-Awp�Aw�Av�Au��As%Ap �An�AnbNAj�`Ai�7Ah��Ah��AhZAg��Ag|�Af��AehsAbn�A_ƨA^{A\�A\Q�A\-A[hsAZ~�AZ1AY�-AX�yAXA�AW�AW�AW`BAW7LAV��AU��AU;dAS+AO��AN-ALz�AK��AK�AJJAIS�AHA�AH  AHM�AG��AFffAE�AEt�AD�AC�hACoAB�yABr�AA��AAG�AA?}A@�A?�A?��A?/A>^5A=O�A:  A9�A9;dA8��A7�A6jA5�A4z�A3��A3l�A37LA3�A2n�A1�A0(�A.��A,�yA+�FA+p�A+K�A*�uA)�^A)�hA)x�A)O�A'��A&�/A&=qA%
=A$�HA$�+A#�
A#�PA#�A#"�A"jA"  A!&�A��A�AM�A�7A?}A?}A"�A��A$�A��A�A�jAZAp�A�9A�+AjAE�A5?AJA��A\)A�AE�A"�Ar�A�
A�A��A1'A�Ap�A�!AA
=A
1'A	�A	/A�A9XA��A��AO�A-A��AdZA+A�`AQ�A �@�C�@��7@�b@��#@�r�@�ƨ@��P@��@�dZ@�ȴ@�x�@�@�$�@�@��@�
=@��#@�Ĝ@�w@�E�@�(�@��@�v�@�$�@���@�I�@�-@��;@��H@�-@ݺ^@�`B@�?}@�/@��@���@��@��`@�Ĝ@�A�@��H@���@҇+@�Ĝ@Ͼw@�`B@� �@���@�K�@�^5@�x�@��`@��/@���@�Z@�v�@�p�@�7L@��@ļj@�I�@�b@��
@å�@�S�@��@��@�@�X@��@��
@��P@�|�@��@�M�@��!@��m@�@��H@��R@�^5@��@��#@�x�@�%@��/@��9@�j@�A�@�9X@�1'@�1'@�  @�dZ@�V@�V@���@��R@�G�@�
=@�ȴ@�ff@���@���@�j@��@���@���@�-@�@�hs@��@�l�@��\@��@���@��@��@��@��@�$�@���@��h@�hs@��@�%@��9@�9X@��
@���@�\)@�@��+@�{@��h@�&�@��9@�r�@��@��w@�;d@��@�(�@�ƨ@��P@�t�@�S�@�"�@��@���@�V@�{@�@�`B@���@�(�@�dZ@�@��R@���@�V@�E�@�@��-@�`B@���@��9@�r�@�Q�@�b@��m@�ƨ@���@�l�@�\)@�C�@�
=@��H@���@�E�@�`B@�&�@��@�V@���@���@��`@���@���@���@�Ĝ@��j@��j@�Ĝ@�Ĝ@��9@��@���@���@��@�z�@�bN@�A�@�@}@|�/@|�@z�@z=q@z�@zJ@y��@y�#@y��@y7L@y�@y%@x��@x��@xbN@xb@w�P@w
=@v�R@v5?@t��@t�j@tj@s��@s��@s�@sC�@r~�@rM�@r�@q�#@qG�@p��@pQ�@o�w@o\)@n�y@nȴ@n�+@n5?@m@m�-@m�-@m��@m�h@mp�@m/@mV@l��@l��@k�F@j�H@j��@j�\@jJ@i�@h��@h�`@h�`@h��@h��@hr�@hb@g�;@gK�@fV@f$�@f@e�@e�T@e@e�h@e`B@eV@d��@c��@b�@b��@b��@b^5@b�@a�#@a�^@a�^@a�^@a�^@a��@a�@`r�@`1'@` �@`  @_��@_l�@_K�@_
=@_
=@^��@^5?@]�-@]`B@\�@\I�@[dZ@Zn�@Z�@Y��@Y��@YX@Y7L@Y�@Y�@Y&�@Y&�@Y�@Y�@Y&�@Y�@XĜ@X�@X�@XbN@XA�@Xb@V�y@U�-@U�@UV@T��@UO�@T�@S�
@S�
@S��@SC�@S@R��@Rn�@RM�@Q�@Q��@Qx�@Qhs@QX@QG�@P��@Pr�@P �@Pb@O�;@O�@O�P@Ol�@O\)@OK�@O+@N��@N��@N��@N�y@N��@N��@N�y@N�@Nȴ@N��@N��@N�+@M�@M`B@MV@K�m@K"�@K@J��@I�^@H�9@H��@H��@H�u@H�u@H�u@H�u@H�u@H�u@H�u@H�u@H�u@G�w@G��@GK�@G
=@F�@F��@Fv�@FV@F@E�T@E@E`B@D(�@CC�@A�7@@�u@@ �@@  @?\)@?�@>5?@=�T@=��@=��@=@=�-@=�h@=/@<��@<��@<�D@;�
@;��@;��@;S�@;"�@:�@:��@:��@:~�@9�^@9�@8bN@7��@7;d@6�@6�R@6��@6{@3ƨ@2�@2~�@2M�@2-@2�@2�@2J@1��@1�#@1��@1��@1hs@17L@1�@1%@0��@0�u@0r�@0bN@0bN@0A�@01'@/�@.��@.�@.��@.V@.5?@-�@,�j@+��@+dZ@+dZ@+dZ@+C�@*��@*��@*n�@*-@*J@)�#@)��@)��@)�7@)hs@)&�@(�9@(�u@(r�@(bN@(A�@( �@(  @'�w@'�P@'\)@'
=@&�@&v�@&V@&E�@&5?@&5?@&$�@&$�@&{@&@%�T@%��@%@%�-@%?}@$��@$�@$�D@$�D@$z�@$j@$I�@#C�@"�!@"�\@"n�@"=q@!G�@ Q�@ r�@ bN@ bN@ Q�@ A�@ 1'@ b@   @�@�@   @�@�;@�;@��@��@�w@l�@��@�@�-@`B@�j@Z@9X@(�@1@t�@"�@"�@"�@o@�@�!@��@M�@�@�@-@-@�@�@��@��@�@��@�^@�^@��@�^@��@��@��@�7@�7@x�@x�@x�@x�@x�@hs@G�@7L@&�@�@�@�@�@��@��@Ĝ@�9@��@A�@b@�@��@�@��@�P@�P@|�@l�@\)@l�@+@��@��@{@@��@p�@V@�@��@�F@��@�@dZ@33@@�H@��@��@�\@M�@-@�@��@�@�#@��@�^@��@��@��@��@�7@�7@�7@x�@hs@G�@r�@�@��@|�@l�@\)@\)@\)@\)@K�@K�@+@�y@ȴ@E�@�T@��@@�@V@�@�D@I�@(�@1@�
@�F@��@t�@C�@o@
�H@
��@
~�@
M�@
-@	��@	&�@Ĝ@��@�u@�u@�@r�@bN@Q�@b@��@
=@�y@��@�+@�+@ff@5?@@@��@p�@V@��@9X@��@dZ@33@��@��@�\@n�@=q@-@-@�@J@��@�@��@��@��@�^@�^@�^@�7@x�@�7@�7@�7@�7@��@��@��@��@��@��@��@hs@G�@7L@7L@ �`@ Ĝ@ �u@ �@ bN@ 1'@  �@ b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��`AăAć+AąAăAć+AċDAĉ7Ać+AăAāAć+Ać+AąAăA�|�A�z�A�|�A�|�A�v�A�=qA�ĜAç�A�t�A�I�A�+A��A�A�n�A�$�A��A��A��^A��`A��-A�=qA�=qA��HA�/A���A��yA�ZA��FA���A�A��A�hsA�&�A��A�t�A�I�A��A�VA��/A�
=A�1A��A��HA���A�hsA�"�A���A�%A�;dA��;A��TA�?}A�bA�&�A���A�1A���A��;A�bA�$�A�Q�A�A/A}�Az(�Ay��Ayp�AyG�Ay�Ax��Ax-Awp�Aw�Av�Au��As%Ap �An�AnbNAj�`Ai�7Ah��Ah��AhZAg��Ag|�Af��AehsAbn�A_ƨA^{A\�A\Q�A\-A[hsAZ~�AZ1AY�-AX�yAXA�AW�AW�AW`BAW7LAV��AU��AU;dAS+AO��AN-ALz�AK��AK�AJJAIS�AHA�AH  AHM�AG��AFffAE�AEt�AD�AC�hACoAB�yABr�AA��AAG�AA?}A@�A?�A?��A?/A>^5A=O�A:  A9�A9;dA8��A7�A6jA5�A4z�A3��A3l�A37LA3�A2n�A1�A0(�A.��A,�yA+�FA+p�A+K�A*�uA)�^A)�hA)x�A)O�A'��A&�/A&=qA%
=A$�HA$�+A#�
A#�PA#�A#"�A"jA"  A!&�A��A�AM�A�7A?}A?}A"�A��A$�A��A�A�jAZAp�A�9A�+AjAE�A5?AJA��A\)A�AE�A"�Ar�A�
A�A��A1'A�Ap�A�!AA
=A
1'A	�A	/A�A9XA��A��AO�A-A��AdZA+A�`AQ�A �@�C�@��7@�b@��#@�r�@�ƨ@��P@��@�dZ@�ȴ@�x�@�@�$�@�@��@�
=@��#@�Ĝ@�w@�E�@�(�@��@�v�@�$�@���@�I�@�-@��;@��H@�-@ݺ^@�`B@�?}@�/@��@���@��@��`@�Ĝ@�A�@��H@���@҇+@�Ĝ@Ͼw@�`B@� �@���@�K�@�^5@�x�@��`@��/@���@�Z@�v�@�p�@�7L@��@ļj@�I�@�b@��
@å�@�S�@��@��@�@�X@��@��
@��P@�|�@��@�M�@��!@��m@�@��H@��R@�^5@��@��#@�x�@�%@��/@��9@�j@�A�@�9X@�1'@�1'@�  @�dZ@�V@�V@���@��R@�G�@�
=@�ȴ@�ff@���@���@�j@��@���@���@�-@�@�hs@��@�l�@��\@��@���@��@��@��@��@�$�@���@��h@�hs@��@�%@��9@�9X@��
@���@�\)@�@��+@�{@��h@�&�@��9@�r�@��@��w@�;d@��@�(�@�ƨ@��P@�t�@�S�@�"�@��@���@�V@�{@�@�`B@���@�(�@�dZ@�@��R@���@�V@�E�@�@��-@�`B@���@��9@�r�@�Q�@�b@��m@�ƨ@���@�l�@�\)@�C�@�
=@��H@���@�E�@�`B@�&�@��@�V@���@���@��`@���@���@���@�Ĝ@��j@��j@�Ĝ@�Ĝ@��9@��@���@���@��@�z�@�bN@�A�@�@}@|�/@|�@z�@z=q@z�@zJ@y��@y�#@y��@y7L@y�@y%@x��@x��@xbN@xb@w�P@w
=@v�R@v5?@t��@t�j@tj@s��@s��@s�@sC�@r~�@rM�@r�@q�#@qG�@p��@pQ�@o�w@o\)@n�y@nȴ@n�+@n5?@m@m�-@m�-@m��@m�h@mp�@m/@mV@l��@l��@k�F@j�H@j��@j�\@jJ@i�@h��@h�`@h�`@h��@h��@hr�@hb@g�;@gK�@fV@f$�@f@e�@e�T@e@e�h@e`B@eV@d��@c��@b�@b��@b��@b^5@b�@a�#@a�^@a�^@a�^@a�^@a��@a�@`r�@`1'@` �@`  @_��@_l�@_K�@_
=@_
=@^��@^5?@]�-@]`B@\�@\I�@[dZ@Zn�@Z�@Y��@Y��@YX@Y7L@Y�@Y�@Y&�@Y&�@Y�@Y�@Y&�@Y�@XĜ@X�@X�@XbN@XA�@Xb@V�y@U�-@U�@UV@T��G�O�@T�@S�
@S�
@S��@SC�@S@R��@Rn�@RM�@Q�@Q��@Qx�@Qhs@QX@QG�@P��@Pr�@P �@Pb@O�;@O�@O�P@Ol�@O\)@OK�@O+@N��@N��@N��@N�y@N��@N��@N�y@N�@Nȴ@N��@N��@N�+@M�@M`B@MV@K�m@K"�@K@J��@I�^@H�9@H��@H��@H�u@H�u@H�u@H�u@H�u@H�u@H�u@H�u@H�u@G�w@G��@GK�@G
=@F�@F��@Fv�@FV@F@E�T@E@E`B@D(�@CC�@A�7@@�u@@ �@@  @?\)@?�@>5?@=�T@=��@=��@=@=�-@=�h@=/@<��@<��@<�D@;�
@;��@;��@;S�@;"�@:�@:��@:��@:~�@9�^@9�@8bN@7��@7;d@6�@6�R@6��@6{@3ƨ@2�@2~�@2M�@2-@2�@2�@2J@1��@1�#@1��@1��@1hs@17L@1�@1%@0��@0�u@0r�@0bN@0bN@0A�@01'@/�@.��@.�@.��@.V@.5?@-�@,�j@+��@+dZ@+dZ@+dZ@+C�@*��@*��@*n�@*-@*J@)�#@)��@)��@)�7@)hs@)&�@(�9@(�u@(r�@(bN@(A�@( �@(  @'�w@'�P@'\)@'
=@&�@&v�@&V@&E�@&5?@&5?@&$�@&$�@&{@&@%�T@%��@%@%�-@%?}@$��@$�@$�D@$�D@$z�@$j@$I�@#C�@"�!@"�\@"n�@"=q@!G�@ Q�@ r�@ bN@ bN@ Q�@ A�@ 1'@ b@   @�@�@   @�@�;@�;@��@��@�w@l�@��@�@�-@`B@�j@Z@9X@(�@1@t�@"�@"�@"�@o@�@�!@��@M�@�@�@-@-@�@�@��@��@�@��@�^@�^@��@�^@��@��@��@�7@�7@x�@x�@x�@x�@x�@hs@G�@7L@&�@�@�@�@�@��@��@Ĝ@�9@��@A�@b@�@��@�@��@�P@�P@|�@l�@\)@l�@+@��@��@{@@��@p�@V@�@��@�F@��@�@dZ@33@@�H@��@��@�\@M�@-@�@��@�@�#@��@�^@��@��@��@��@�7@�7@�7@x�@hs@G�@r�@�@��@|�@l�@\)@\)@\)@\)@K�@K�@+@�y@ȴ@E�@�T@��@@�@V@�@�D@I�@(�@1@�
@�F@��@t�@C�@o@
�H@
��@
~�@
M�@
-@	��@	&�@Ĝ@��@�u@�u@�@r�@bN@Q�@b@��@
=@�y@��@�+@�+@ff@5?@@@��@p�@V@��@9X@��@dZ@33@��@��@�\@n�@=q@-@-@�@J@��@�@��@��@��@�^@�^@�^@�7@x�@�7@�7@�7@�7@��@��@��@��@��@��@��@hs@G�@7L@7L@ �`@ Ĝ@ �u@ �@ bN@ 1'@  �@ b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�{BoBoBoBhBoBoBoBhBhBhBoBhBhBhBbBbBbB\B\BDBBB��B��B��B��B��B�B�B�fB��B�XB�{Bp�B^5BK�B.B�BoBPB1BB��B��B�B�B��BǮB�qB�^B�LB�FB�-B��B�uB�B}�Bz�Bw�Bu�Br�BiyB_;BN�BB�B:^B)�BhB  B
�B
�5B
��B
ǮB
�^B
�B
��B
�{B
�1B
q�B
m�B
l�B
k�B
iyB
gmB
cTB
^5B
[#B
XB
O�B
:^B
'�B
�B
�B
B	��B	�B	�B	�B	�B	�yB	�NB	�
B	ÖB	�9B	��B	��B	��B	��B	��B	�uB	�\B	�PB	�1B	�B	�B	�B	� B	}�B	y�B	s�B	n�B	aHB	N�B	E�B	>wB	9XB	5?B	1'B	,B	'�B	'�B	-B	-B	'�B	$�B	$�B	%�B	$�B	#�B	#�B	!�B	�B	�B	�B	�B	oB	bB	PB	
=B	B��B��B��B��B�B�B�fB�TB�HB�;B�5B�)B�B�B��BȴBB�}B�qB�jB�^B�LB�FB�?B�-B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�B{�By�Bx�Bx�Bw�Bv�Bv�Bu�Bt�Bs�Br�Bs�Bt�Bt�Bs�Bs�Br�Bq�Bp�Bo�Bn�Bl�BjBhsBgmBe`BdZBcTBbNB`BB_;B]/B\)B[#BZBYBW
BT�BR�BQ�BP�BN�BM�BM�BL�BJ�BH�BE�BB�BA�B?}B?}B?}B>wB>wB>wB=qB<jB;dB:^B:^B9XB8RB8RB7LB6FB5?B33B2-B2-B2-B1'B1'B0!B0!B1'B1'B1'B1'B1'B1'B0!B0!B0!B0!B0!B/B.B,B,B+B+B)�B+B.B.B.B/B/B/B/B/B.B/B0!B0!B0!B0!B0!B1'B1'B1'B1'B2-B2-B33B33B2-B5?B5?B49B33B/B5?B=qB>wB>wB>wB>wB?}B?}B@�BA�BA�BB�BB�BC�BC�BC�BB�BB�BC�BF�BK�BR�BVBYB_;B_;B`BBaHBe`BgmBhsBjBl�Bo�Bo�Bq�Bv�Bx�B|�B~�B� B�B�%B�+B�=B�VB�hB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�XBƨBȴBɺB��B��B��B��B��B��B��B��B�
B�B�;B�`B�mB�yB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	1B	
=B	hB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	'�B	,B	/B	33B	6FB	6FB	7LB	7LB	7LB	8RB	:^B	<jB	>wB	?}B	C�B	E�B	F�B	I�B	K�B	M�B	M�B	N�B	N�B	O�B	O�B	Q�B	R�B	T�B	ZB	\)B	]/B	_;B	bNB	dZB	hsB	k�B	m�B	o�B	p�B	q�B	s�B	v�B	v�B	v�B	v�B	w�B	w�B	x�B	x�B	y�B	z�B	~�B	�B	�%B	�%B	�=B	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�9B	�9B	�9B	�FB	�RB	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�/B	�)B	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
VB
\B
bB
bB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
$�B
$�B
)�B
+B
,B
,B
,B
,B
-B
,B
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
1'B
1'B
1'B
1'B
1'B
2-B
33B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
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
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
C�B
C�B
C�B
C�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
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
P�B
P�B
P�B
P�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
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
W
B
XB
XB
XB
YB
YB
ZB
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
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
dZB
dZB
dZB
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
iyB
jB
jB
k�B
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
n�B
n�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�{BoBoBoBhBoBoBoBhBhBhBoBhBhBhBbBbB}B�B�BBgBuB�cB�BB�qB�jB��B��B�B��B�qB�uB��BxBiBZB6�B�BFB�B	�BgB��B�B�oB��BΊBɆB��B��B��B�fB��B�"B��B�mB~�B{Bx�Bv�Bt�Bk�Bb4BQ4BD�B=�B.BFB�B
��B
�'B
�B
��B
��B
��B
�B
��B
�DB
raB
m�B
l�B
k�B
i�B
h>B
d&B
^�B
[�B
Y�B
S[B
=qB
)�B
!HB
B
�B	��B	�B	�-B	�AB	�}B	��B	�tB	چB	ƎB	�+B	�KB	��B	�5B	��B	��B	�B	��B	�<B	�B	��B	�{B	�oB	�iB	~�B	{B	uB	q�B	d�B	P�B	GzB	?cB	:xB	6�B	2B	-)B	(>B	($B	.B	.cB	(�B	%�B	%�B	'8B	%zB	$@B	$�B	"�B	5B	�B	�B	?B	B	NB	�B	0B	KB��B�XB��B�+B�|B�B�B�B��BߤB޸B�/B�kB��BбB�B��B� B��B�qB�JB��B��B��B�B�DB��B�B�B�IB�xB�B��B�WB��B��B�YB��B��B��B|�BzDBy	By$BxlBw�BwfBv`Bu�Bt�Bs�Bt�BuBt�BtBs�BsBr-Bq[BpoBo�Bm�Bk�Bi_BhsBfBeBc�Bc:BaHB`\B^�B]IB\BZ�BY�BW�BVBT�BS�BR:BO�BNVBNVBM�BK�BJ�BG_BC�BB�B@�B@iB?�B>�B>�B>�B>(B=�B<�B;JB:�B:*B9XB9>B8B72B6zB4�B3B2aB2�B1�B2|B1�B1[B1�B1�B1�B1vB1[B1AB0UB0UB0;B0UB0�B/�B/�B/B./B,=B,B+�B+�B.}B.�B.�B/�B/�B/OB/iB/�B/OB/�B0UB0UB0oB0�B0oB1vB1vB1�B1�B2�B2�B3�B3�B2�B5�B5�B5B5%B4B8�B=�B>�B>�B>�B>�B?�B?�B@�BA�BA�BB�BB�BC�BC�BC�BB�BC-BD�BG�BL�BS�BW
BZkB_�B_�B`�Ba�Be�Bg�BiBkBm)Bo�Bp;Br�BwfByrB}qBcB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�4B�@B�RB�_B�QB�qB�}B��B�3B�^B��B��B��B��B��B�B�6B�B�4B�@B�aB�sBٴB߾B�B�B�B�B�B��B��B��B��B�B��B�B�"B�(B�.B	 4B	AB	-B	GB	MB	SB	tB	�B	
�B	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	/B	 vB	(>B	,=B	/�B	3hB	6`B	6`B	7LB	7fB	7�B	8�B	:xB	<�B	>wB	?�B	C�B	E�B	F�B	I�B	K�B	N"B	N<B	N�B	OB	PB	O�B	RB	S&B	UMB	Z7B	\CB	]dB	_pB	b�B	d�B	h�B	k�B	m�B	o�B	p�B	q�B	s�B	v�B	v�B	v�B	v�B	w�B	w�B	x�B	x�B	zB	{JB	HB	�3B	�%B	�tB	��B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�LB	�6B	�/B	�/B	�/B	�5B	�;B	�GB	�3B	�9B	�TB	�TB	�nB	�zB	�lB	�rB	�rB	�rB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	� B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�,B	�{B	�_B	�=B	�CB	�IB	�/G�O�B	�\B	�\B	�\B	�bB	�hB	�B	�nB	�tB	�B	�zB	�fB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�}B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�8B	�0B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�HB
 B
 B
 B
'B
B
-B
-B
3B
3B
9B
mB
�B
�B
	�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
!B
!�B
#B
#�B
$�B
$�B
%,B
%�B
*0B
+B
,"B
,"B
,B
+�B
-B
,B
-)B
-)B
-)B
-)B
-)B
./B
./B
./B
./B
./B
/ B
/B
/5B
/5B
/OB
/OB
1AB
1AB
1AB
1'B
1vB
2|B
3�B
5ZB
5?B
5?B
5?B
5tB
6`B
6`B
7fB
7�B
7LB
8RB
8lB
8lB
8lB
8lB
8�B
9rB
9XB
:xB
:xB
:xB
:xB
:xB
;B
;B
;dB
<�B
<jB
<�B
=qB
=qB
=VB
=qB
=qB
=qB
=qB
=�B
=�B
>wB
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
C�B
C�B
C�B
C�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
MB
M�B
N�B
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
P�B
P�B
Q B
P�B
P�B
Q B
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
Q�B
RB
RB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
SB
R�B
R�B
SB
SB
TB
TB
TB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
T�B
V9B
VB
W$B
X+B
X+B
XEB
YKB
YKB
Z7B
[=B
[#B
[=B
[WB
[WB
\CB
\B
\CB
\)B
\)B
]/B
]/B
]IB
]/B
]/B
^5B
^5B
^5B
^5B
^B
^5B
^5B
^B
^5B
^5B
^OB
^OB
^�B
_VB
`\B
aHB
a-B
aHB
aHB
a-B
aHB
aHB
aHB
abB
bhB
b�B
b�B
cnB
cnB
dtB
dtB
d�B
ezB
ezB
ezB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
k�B
k�B
kkB
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
n�B
n�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<-��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711100034142017111000341420171110003414202211182132232022111821322320221118213223201804031937552018040319375520180403193755  JA  ARFMdecpA19c                                                                20171031003507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171030153508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171030153510  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171030153510  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171030153511  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171030153511  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171030153511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171030153511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171030153511  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171030153511                      G�O�G�O�G�O�                JA  ARUP                                                                        20171030155541                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171101153449  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20171102000000  CF  PSAL_ADJUSTED_QCD�@ D�@ G�O�                JM  ARSQJMQC2.0                                                                 20171102000000  CF  TEMP_ADJUSTED_QCD�@ D�@ G�O�                JM  ARCAJMQC2.0                                                                 20171109153414  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171109153414  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103755  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171534                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123223  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                