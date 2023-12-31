CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-06-12T15:37:40Z creation;2019-06-12T15:37:45Z conversion to V3.1;2019-12-18T07:14:31Z update;2022-11-21T05:28:49Z update;     
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
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190612153740  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_176                     2C  DdE�NAVIS_A                         0397                            ARGO 011514                     863 @��!���1   @��!З� @;������dE��(1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A���A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��{@��H@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A��A��A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dl)D�)Dr�D�Dr�D�Dr�D�Dr�D�Dx�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�B�D�b�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��!A�dZA�dZA�hsA�l�A�jA�l�A�jA�l�A�S�A��A�A�A���A�+A��`A�z�A�{A�S�A�oA�5?A�JA��PA�E�A�A�ƨA��DA���A�\)A���A���A��A���A��A��!A�`BA�O�A�?}A�7LA�(�A��HA�/A��A�ȴA�ffA��A��RA��A�M�A� �A��/A�|�A�?}A��A��/A��^A�\)A�JA��PA�5?A�+A�&�A��A���A��A�+A��A��A�A���A��A�/A��FA��DA�ZA�l�A�A~��A|^5A{VAzI�AyAwx�Av�AtĜAt  As`BAs33ArbNAq%Ao��Am�mAmC�Al��Ak�TAk;dAj�jAj5?AhjAe7LAdAc�Abr�A`��A_�hA^�A]`BA\^5A[�FAZ�AY�FAXI�AW7LAU\)AS��AR�+AQ��AQAOl�AN��AM��AMoAL�9ALffAL$�AK�wAK`BAJ�HAI��AH��AH  AGK�AFĜAFbNAE�mAEl�AE7LAD-AB��AA�AA"�A@r�A?"�A>E�A=�mA=;dA<�jA<�A;�PA:ȴA:��A:ZA9��A9?}A8��A7�wA5��A4��A3p�A2��A1dZA0ffA/XA.�9A-�
A-�7A-�A,�/A,�+A+�
A*�RA);dA(�A(��A(jA'�-A&VA%/A$�\A$(�A#XA"^5A!��A!`BA �HA �jA��A�AĜAM�A�A�A+Ar�A �AAS�A�DA�AG�A%A��AE�A`BAȴA�A�DA"�A~�A-A��AA��A1'A�wA�A��A�PAl�A7LA�A �A\)A
ZA(�AĜA=qA�yA
=AjA J@�ȴ@��@���@���@��7@�O�@�bN@�l�@�"�@�=q@�z�@���@�&�@�K�@�Ĝ@���@�\)@�5?@�9X@��@��@�
=@�j@�X@�@�S�@�^5@��#@�&�@�r�@��
@�dZ@�
=@�ȴ@�E�@���@�O�@���@Ώ\@̓u@�+@�5?@���@�Z@�1'@���@ư!@ź^@�Ĝ@î@�C�@�
=@���@�j@�;d@���@��@���@�C�@��+@���@��^@��^@�x�@�?}@��@�ff@��@��h@�?}@�%@���@���@��D@�Z@�9X@�  @�M�@�?}@�bN@���@�o@��R@�=q@���@���@���@��D@�Q�@��P@�-@���@��@��^@��^@��7@��@��F@���@�S�@���@��-@�&�@��w@�"�@���@�{@���@��D@��;@�33@���@�?}@���@���@��\@��T@��7@��@��@��@�  @�ƨ@�+@���@�M�@�-@�@�@�p�@�O�@�7L@��@��@�(�@�33@��@��H@�ȴ@���@���@���@��@�Z@�Q�@�I�@�9X@�  @�dZ@�
=@�~�@�5?@��7@�7L@�Ĝ@��@��w@��@���@��@��@���@�G�@�z�@�I�@��@�ƨ@�l�@�K�@�33@�"�@�@��@��!@��+@�v�@�^5@�5?@���@��@��@���@���@��9@��u@�r�@�bN@�A�@� �@�  @��F@���@�\)@�o@�@��H@�ȴ@�v�@���@���@��@��9@���@� �@~��@~ff@~{@~@}`B@|�j@|j@|�@{t�@{"�@{"�@z�@z�H@zn�@zJ@y�#@y�^@y��@yx�@y�@xĜ@xr�@w�;@w��@w�w@w�w@w�@w��@wl�@w;d@v��@v�R@vV@u�T@u��@u�h@u`B@t��@t1@s��@sC�@r�H@r�@p�9@o��@o�@n��@nV@n5?@n@m�T@m�-@m��@m�@mp�@mp�@m`B@m?}@mV@l��@l��@l��@l��@l��@l��@k��@j�\@jn�@jM�@jM�@j-@j�@i�#@i��@ix�@ihs@iX@i7L@i�@h�9@hr�@hb@gK�@f��@f�@fff@e@ep�@e`B@eO�@eO�@e/@d�@dI�@c�m@cS�@co@b��@bM�@bJ@a��@ahs@a%@`r�@_�@_l�@_
=@^��@^5?@]�@]��@]�@]`B@]?}@]V@\�/@\�D@\I�@\I�@\9X@\(�@[�
@[�F@[�F@[��@[t�@[dZ@[dZ@[dZ@[dZ@[S�@[33@Z��@ZM�@ZJ@Y��@Y�7@YX@Y�@X��@W�;@W+@V��@U`B@T��@T�@T�j@T�D@T(�@SC�@R�@R��@R��@R�!@R�!@Rn�@RJ@Q��@Q�^@Qx�@QG�@Q%@P�`@P��@P�@Pr�@PbN@PA�@PA�@P  @O�@O�P@O�P@O\)@OK�@O+@N�R@Nff@Mp�@M�@L�D@LI�@L�@K��@K�@J��@J^5@I��@I�^@I�^@I��@IX@IG�@H�`@H�9@H�u@Hr�@G��@GK�@GK�@G+@F�y@Fff@F5?@F@E��@E@E@E@E@Ep�@D��@D��@D9X@C�@CdZ@C"�@B^5@A��@Ax�@A�7@A��@A��@A%@@bN@@Q�@@Q�@@Q�@@b@?�@?K�@>ȴ@>��@>��@>��@>��@>�+@>v�@>ff@>5?@=p�@;�m@;��@;t�@;33@:�@:��@:��@:��@:~�@:^5@:M�@:�@9�#@9�^@9�7@9hs@97L@9&�@9&�@9%@8Ĝ@8�u@8 �@7��@7|�@7K�@7;d@7+@7
=@6��@65?@6@5��@5V@4�/@4z�@3�
@3�F@3��@3��@3dZ@2��@2J@1hs@0�`@0�9@0�@01'@0  @/�@/�@/�P@.��@.ff@.E�@.E�@.E�@.5?@.$�@.$�@.$�@.$�@.5?@.$�@.{@-�@-?}@,�@,Z@+�
@+ƨ@+��@+33@+@*��@*��@*��@*^5@*-@)��@)�^@)&�@(�`@(Q�@(Q�@(1'@'�w@';d@&�y@&�@&�R@&v�@&@%@%p�@$z�@$�@$1@#�@"��@"^5@"-@!�@!��@!7L@ �`@ �9@ �@ A�@   @�;@��@|�@�@
=@��@��@�@�+@V@5?@$�@{@{@�@�T@@�@�j@�@�D@Z@�@1@��@ƨ@��@�@dZ@@��@��@��@��@��@��@�!@&�@r�@ �@�;@��@�P@|�@l�@K�@
=@�@��@��@��@��@��@v�@ff@�@p�@��@��@I�@�@��@ƨ@ƨ@�
@ƨ@��@��@��@��@�@t�@dZ@33@"�@@�@�@�H@��@��@~�@~�@^5@�@J@�^@hs@7L@�9@�@A�@  @��@+@��@��@�y@�@�@�R@�+@ff@E�@5?@$�@$�@@�T@p�@�@��@�/@�j@�@z�@�@�m@dZ@33@"�@"�@@@
�H@
�\@	��@	��@	��@	G�@	7L@	&�@�`@��@bN@b@�;@�;@�;@�;@��@��@l�@\)@\)@\)@K�@+@�@�y@�+@V@5?@{@@��@�-@��@�h@`B@��@�@�D@�D@z�@z�@j@j@j@j@9X@��@�
@��@dZ@C�@33@�@�H@�!@n�@^5@=q@�@��@��@�^@��@�7@x�@x�@x�@x�@x�@7L@%@ �`@ �`@ Ĝ@ �u@ bN@ b?�|�?��?��R?��R?�v�?�5??��?���?��-?�p�?�/?��?��D?�I�?�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��!A�dZA�dZA�hsA�l�A�jA�l�A�jA�l�A�S�A��A�A�A���A�+A��`A�z�A�{A�S�A�oA�5?A�JA��PA�E�A�A�ƨA��DA���A�\)A���A���A��A���A��A��!A�`BA�O�A�?}A�7LA�(�A��HA�/A��A�ȴA�ffA��A��RA��A�M�A� �A��/A�|�A�?}A��A��/A��^A�\)A�JA��PA�5?A�+A�&�A��A���A��A�+A��A��A�A���A��A�/A��FA��DA�ZA�l�A�A~��A|^5A{VAzI�AyAwx�Av�AtĜAt  As`BAs33ArbNAq%Ao��Am�mAmC�Al��Ak�TAk;dAj�jAj5?AhjAe7LAdAc�Abr�A`��A_�hA^�A]`BA\^5A[�FAZ�AY�FAXI�AW7LAU\)AS��AR�+AQ��AQAOl�AN��AM��AMoAL�9ALffAL$�AK�wAK`BAJ�HAI��AH��AH  AGK�AFĜAFbNAE�mAEl�AE7LAD-AB��AA�AA"�A@r�A?"�A>E�A=�mA=;dA<�jA<�A;�PA:ȴA:��A:ZA9��A9?}A8��A7�wA5��A4��A3p�A2��A1dZA0ffA/XA.�9A-�
A-�7A-�A,�/A,�+A+�
A*�RA);dA(�A(��A(jA'�-A&VA%/A$�\A$(�A#XA"^5A!��A!`BA �HA �jA��A�AĜAM�A�A�A+Ar�A �AAS�A�DA�AG�A%A��AE�A`BAȴA�A�DA"�A~�A-A��AA��A1'A�wA�A��A�PAl�A7LA�A �A\)A
ZA(�AĜA=qA�yA
=AjA J@�ȴ@��@���@���@��7@�O�@�bN@�l�@�"�@�=q@�z�@���@�&�@�K�@�Ĝ@���@�\)@�5?@�9X@��@��@�
=@�j@�X@�@�S�@�^5@��#@�&�@�r�@��
@�dZ@�
=@�ȴ@�E�@���@�O�@���@Ώ\@̓u@�+@�5?@���@�Z@�1'@���@ư!@ź^@�Ĝ@î@�C�@�
=@���@�j@�;d@���@��@���@�C�@��+@���@��^@��^@�x�@�?}@��@�ff@��@��h@�?}@�%@���@���@��D@�Z@�9X@�  @�M�@�?}@�bN@���@�o@��R@�=q@���@���@���@��D@�Q�@��P@�-@���@��@��^@��^@��7@��@��F@���@�S�@���@��-@�&�@��w@�"�@���@�{@���@��D@��;@�33@���@�?}@���@���@��\@��T@��7@��@��@��@�  @�ƨ@�+@���@�M�@�-@�@�@�p�@�O�@�7L@��@��@�(�@�33@��@��H@�ȴ@���@���@���@��@�Z@�Q�@�I�@�9X@�  @�dZ@�
=@�~�@�5?@��7@�7L@�Ĝ@��@��w@��@���@��@��@���@�G�@�z�@�I�@��@�ƨ@�l�@�K�@�33@�"�@�@��@��!@��+@�v�@�^5@�5?@���@��@��@���@���@��9@��u@�r�@�bN@�A�@� �@�  @��F@���@�\)@�o@�@��H@�ȴ@�v�@���@���@��@��9@���@� �@~��@~ff@~{@~@}`B@|�j@|j@|�@{t�@{"�@{"�@z�@z�H@zn�@zJ@y�#@y�^@y��@yx�@y�@xĜ@xr�@w�;@w��@w�w@w�w@w�@w��@wl�@w;d@v��@v�R@vV@u�T@u��@u�h@u`B@t��@t1@s��@sC�@r�H@r�@p�9@o��@o�@n��@nV@n5?@n@m�T@m�-@m��@m�@mp�@mp�@m`B@m?}@mV@l��@l��@l��@l��@l��@l��@k��@j�\@jn�@jM�@jM�@j-@j�@i�#@i��@ix�@ihs@iX@i7L@i�@h�9@hr�@hb@gK�@f��@f�@fff@e@ep�@e`B@eO�@eO�@e/@d�@dI�@c�m@cS�@co@b��@bM�@bJ@a��@ahs@a%@`r�@_�@_l�@_
=@^��@^5?@]�@]��@]�@]`B@]?}@]V@\�/@\�D@\I�@\I�@\9X@\(�@[�
@[�F@[�F@[��@[t�@[dZ@[dZ@[dZ@[dZ@[S�@[33@Z��@ZM�@ZJ@Y��@Y�7@YX@Y�@X��@W�;@W+@V��@U`B@T��@T�@T�j@T�D@T(�@SC�@R�@R��@R��@R�!@R�!@Rn�@RJ@Q��@Q�^@Qx�@QG�@Q%@P�`@P��@P�@Pr�@PbN@PA�@PA�@P  @O�@O�P@O�P@O\)@OK�@O+@N�R@Nff@Mp�@M�@L�D@LI�@L�@K��@K�@J��@J^5@I��@I�^@I�^@I��@IX@IG�@H�`@H�9@H�u@Hr�@G��@GK�@GK�@G+@F�y@Fff@F5?@F@E��@E@E@E@E@Ep�@D��@D��@D9X@C�@CdZ@C"�@B^5@A��@Ax�@A�7@A��@A��@A%@@bN@@Q�@@Q�@@Q�@@b@?�@?K�@>ȴ@>��@>��@>��@>��@>�+@>v�@>ff@>5?@=p�@;�m@;��@;t�@;33@:�@:��@:��@:��@:~�@:^5@:M�@:�@9�#@9�^@9�7@9hs@97L@9&�@9&�@9%@8Ĝ@8�u@8 �@7��@7|�@7K�@7;d@7+@7
=@6��@65?@6@5��@5V@4�/@4z�@3�
@3�F@3��@3��@3dZ@2��@2J@1hs@0�`@0�9@0�@01'@0  @/�@/�@/�P@.��@.ff@.E�@.E�@.E�@.5?@.$�@.$�@.$�@.$�@.5?@.$�@.{@-�@-?}@,�@,Z@+�
@+ƨ@+��@+33@+@*��@*��@*��@*^5@*-@)��@)�^@)&�@(�`@(Q�@(Q�@(1'@'�w@';d@&�y@&�@&�R@&v�@&@%@%p�@$z�@$�@$1@#�@"��@"^5@"-@!�@!��@!7L@ �`@ �9@ �@ A�@   @�;@��@|�@�@
=@��@��@�@�+@V@5?@$�@{@{@�@�T@@�@�j@�@�D@Z@�@1@��@ƨ@��@�@dZ@@��@��@��@��@��@��@�!@&�@r�@ �@�;@��@�P@|�@l�@K�@
=@�@��@��@��@��@��@v�@ff@�@p�@��@��@I�@�@��@ƨ@ƨ@�
@ƨ@��@��@��@��@�@t�@dZ@33@"�@@�@�@�H@��@��@~�@~�@^5@�@J@�^@hs@7L@�9@�@A�@  @��@+@��@��@�y@�@�@�R@�+@ff@E�@5?@$�@$�@@�T@p�@�@��@�/@�j@�@z�@�@�m@dZ@33@"�@"�@@@
�H@
�\@	��@	��@	��@	G�@	7L@	&�@�`@��@bN@b@�;@�;@�;@�;@��@��@l�@\)@\)@\)@K�@+@�@�y@�+@V@5?@{@@��@�-@��@�h@`B@��@�@�D@�D@z�@z�@j@j@j@j@9X@��@�
@��@dZ@C�@33@�@�H@�!@n�@^5@=q@�@��@��@�^@��@�7@x�@x�@x�@x�@x�@7L@%@ �`@ �`@ Ĝ@ �u@ bN@ b?�|�?��?��R?��R?�v�?�5??��?���?��-?�p�?�/?��?��D?�I�?�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111n�B�B�B�B�B�B�B{B{BuB\BB�ZB�NB�B��B��B��B�B�B�5B��BȴB�?B��B��B��B��B�{B�\B�=B�Bz�Bn�Bm�Bm�Bn�Bn�Bn�Bl�BgmBaHBZBT�BP�BK�BH�BF�BB�B>wB7LB1'B�B��B��B�jB�FB�B��Bq�B]/BP�BJ�BA�B1'B�B
�yB
�/B
ɺB
�FB
��B
��B
��B
��B
�VB
�+B
|�B
k�B
aHB
[#B
Q�B
F�B
=qB
49B
/B
+B
(�B
"�B
�B
bB
B
B	��B	��B	�B	�B	�yB	�5B	��B	ÖB	�}B	�XB	�B	��B	��B	��B	��B	�hB	�DB	�%B	~�B	w�B	o�B	ffB	`BB	ZB	W
B	N�B	J�B	H�B	G�B	F�B	D�B	C�B	A�B	?}B	<jB	7LB	33B	/B	,B	(�B	&�B	$�B	!�B	�B	�B	uB	VB	
=B	B	  B��B��B��B��B�B�B�B�B�B�mB�`B�TB�;B��B��BƨB��B�dB�FB�-B�!B�B��B��B��B��B��B��B��B��B�{B�oB�VB�=B�%B�B�B� B}�Bz�By�By�Bx�Bv�Bu�Bt�Br�Bp�Bo�Bn�Bm�Bl�Bk�BiyBgmBe`BcTBbNBaHB^5B[#BVBR�BO�BM�BK�BJ�BH�BG�BF�BE�BD�BD�BD�BC�BC�BB�B@�B>wB;dB8RB49B1'B/B,B(�B%�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B{BuBuBoBhBbB\BVBPBJBDBJBPBVBVBVBVBVBVB\BVB\B\B\BPB\BhBoBoB{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B �B#�B$�B&�B&�B&�B&�B&�B'�B,B-B.B.B/B/B/B/B/B/B/B33B6FB8RB;dB<jB=qB>wB?}B@�BB�BD�BE�BG�BK�BK�BL�BM�BL�BL�BR�BS�BS�BT�BVBZB[#BaHBcTBdZBgmBhsBm�Bo�Br�By�B|�B� B�B�DB�\B�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�FB�FB�jBBŢBƨBǮBǮBǮBȴB��B��B��B��B�B�B�#B�BB�NB�NB�NB�`B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	%B		7B	
=B	DB	JB	PB	PB	VB	\B	\B	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	'�B	+B	0!B	33B	49B	5?B	8RB	;dB	=qB	?}B	B�B	E�B	F�B	G�B	H�B	K�B	N�B	N�B	O�B	O�B	P�B	S�B	VB	XB	]/B	_;B	_;B	`BB	`BB	aHB	cTB	dZB	ffB	gmB	iyB	l�B	m�B	n�B	n�B	p�B	v�B	y�B	z�B	{�B	� B	�%B	�7B	�DB	�PB	�VB	�\B	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�FB	�RB	�^B	�jB	�jB	�jB	�jB	�jB	�}B	��B	��B	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
1B

=B

=B

=B
DB
DB
JB
PB
PB
PB
VB
VB
VB
PB
VB
VB
VB
\B
bB
bB
hB
oB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
$�B
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
(�B
(�B
)�B
)�B
)�B
+B
-B
-B
-B
.B
.B
/B
/B
/B
0!B
1'B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
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
T�B
VB
T�B
T�B
T�B
XB
YB
YB
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
[#B
[#B
\)B
]/B
]/B
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
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
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
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
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
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
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
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111n�B�B�B�B�B�B�B�B2B2B�B�B�cB��B�MB�^B��B��B��B�B��BּB��B�rB��B�5B�B�+B��B��B�0B�fB}�BoiBm�Bm�Bn�Bo Bo�BnIBh�Bb�B[#BU�BQ�BLdBIRBGEBC{B?�B8�B4�B#�BoB��B��B�8B��B��BtnB_�BRTBL~BDB4nB�B
�B
�;B
�0B
�B
�B
��B
��B
��B
��B
�B
}B
mCB
b�B
\�B
S�B
HfB
>�B
5?B
/�B
+�B
*0B
$�B
�B
:B
%B
�B	�B	��B	�B	��B	�"B	�B	�JB	ĶB	�B	�B	��B	�DB	��B	��B	��B	��B	��B	��B	��B	zB	q�B	h$B	a�B	[WB	X�B	PB	K�B	IlB	HKB	G+B	E9B	DMB	BAB	@�B	=�B	8�B	4TB	0!B	,�B	)�B	'�B	%�B	"�B	!HB	WB	�B	�B	xB	�B	 B��B��B��B��B�B��B�B�=B�B�>B�B��B�B֡BѷB�B�-B��B��B�3B�'B��B�B��B��B�B�nB�pB�+B�B�MB��B�.B��B�+B�B�gB�UB~�B{�Bz�Bz�BzDBw�BvzButBs�Bq'BpUBo�Bn/Bm)BlqBj�Bh�BfBc�BcBb�B`'B^5BW�BT�BQ�BN�BL~BK�BI�BHfBG_BF?BD�BD�BD�BC�BD3BCaBA�B?�B=VB;B6B2|B1AB.}B*�B(�B%B#:B"B�B�BBjBdB)BqB�B�B�B�BB,BB�B�BBbB�BBB�B�B�BB�B�B�B�B�B�B�B�B�B�BBB�BoB[B[B�B�BB�BmBYBYBB1B�B�B�BOBpB�B!�B$�B%zB'8B'8B'RB'�B(
B)B,�B-wB.}B.}B/iB/iB/iB/iB/�B/�B0oB4B72B9	B<B<�B=�B>�B@ BA BC-BE9BFtBH�BK�BLBMBN"BMPBM�BS[BTFBT�BU�BV�BZ�B\)Ba�Bc�BeBh
Bi_BnIBpoBs�BzxB}�B��B�B��B��B��B�B��B��B��B�+B�#B�B�B�B�-B�4B�&B�,B�2B�LB��B��B��B�ZB��B��B�LB�B��B��B��B��B��B�B�RB�6B�VB�hB҉B�mBخB��B��B�B�B��B�B��B�)B�;B�B�B�2B�>B�B�B�B�"B�<B�BB�HB	 4B	 OB	UB	uB	�B	�B		lB	
rB	xB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	 BB	$@B	'8B	(�B	+�B	0�B	3�B	4nB	5�B	8�B	;�B	=�B	?�B	B�B	E�B	F�B	G�B	IB	LB	OB	OB	PB	PB	Q4B	T,B	VSB	XEB	]IB	_pB	_pB	`vB	`vB	a|B	c�B	d�B	f�B	g�B	i�B	l�B	m�B	n�B	n�B	qB	w2B	z*B	{0B	|jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�,B	�B	�B	�$B	�0B	�0B	�6B	�6B	�=B	�WB	�IB	��B	��B	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�B	�)B	�6B	�.B	�:B	�@B	�@B	�2B	�9B	�?B	�EB	�EB	�eB	�KB	�QB	�WB	�CB	�]B	�]B	�]B	�dB	�dB	�dB	�jB	�OB	�OB	�jB	�OB	�OB	�jB	ބB	�vB	�|B	�|B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�	B	�	B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�0B	�6B	�PB	�BB	�HB
 4B
 4B
 4B
 OB
oB
aB
GB
MB
?B
?B
YB
_B
zB
fB
fB
fB
�B

�B

XB

�B
xB
�B
~B
�B
�B
�B
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
CB
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
#B
#B
#B
#B
$B
$B
$B
%B
%B
$�B
$�B
%B
&B
&2B
'B
'B
($B
(
B
($B
)*B
)DB
*0B
*0B
*0B
+kB
-]B
-wB
-]B
./B
.IB
/OB
/OB
/�B
0�B
1vB
2|B
3hB
4nB
4nB
4nB
5tB
5tB
5tB
5�B
7�B
7�B
8lB
8lB
8lB
8lB
8�B
8lB
8lB
8RB
8�B
8�B
8�B
8�B
9�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
FB
G�B
G�B
G�B
IB
I�B
J�B
J�B
J�B
LB
K�B
MB
MB
MB
NB
NB
OB
OB
OB
O�B
PB
O�B
PB
PB
QB
QB
RB
R B
RB
R B
R B
R:B
R:B
S@B
S&B
S&B
S&B
T,B
TB
T,B
T,B
T,B
T,B
U2B
U2B
U2B
UB
VB
UB
VB
U2B
UMB
U�B
X_B
YeB
YKB
Z7B
ZQB
ZQB
ZQB
ZkB
ZQB
[WB
[WB
[=B
[=B
[=B
[WB
[WB
[WB
[qB
\xB
]~B
]dB
^�B
^jB
^jB
^jB
_VB
_VB
_VB
_pB
_pB
_VB
_VB
_pB
_VB
_pB
_pB
_VB
_pB
`\B
`\B
`\B
`\B
`vB
`vB
`vB
`vB
`\B
a|B
a|B
a|B
b�B
b�B
c�B
c�B
c�B
c�B
d�B
ezB
e`B
ezB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
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
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
tB
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
vB
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
y	B
x�B
x�B
y	B
y	B
x�B
x�B
x�B
x�B
y�B
y	B
z*B
z*B
y�B
zB
zB
{B
{B
{0B
|B
|B
|B
}"B
}"B
}"B
}B
}B
}"B
}"B
~(B
~(B
~B
~B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.21(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906230033512019062300335120190623003351202211182139192022111821391920221118213919201906240016292019062400162920190624001629  JA  ARFMdecpA19c                                                                20190613003736  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190612153740  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190612153742  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190612153743  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190612153743  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190612153743  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190612153744  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190612153744  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190612153744  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190612153745                      G�O�G�O�G�O�                JA  ARUP                                                                        20190612155639                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190612153227  CV  JULD            G�O�G�O�F�)	                JM  ARCAJMQC2.0                                                                 20190622153351  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190622153351  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190623151629  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123919  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                