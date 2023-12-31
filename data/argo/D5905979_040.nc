CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:02Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170902  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               (A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؓ�s1   @ؓO�	`@7�Ƨ�c���-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    (A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�%�D�f�D���D���D��D�P�D���D�� D�"�D�b�D���D��RD��D�W\Dڃ�D��D�{D�UqD��D�b=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�G�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw��B��B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�qC{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&�\D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2�\D3\D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE�\DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ�DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dy��D�">D�c3D��D��>D� D�MD��3D��{D�
D�_
D�� D���D�\D�S�Dڀ D��D��D�Q�D�\D�^�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�  A�A�A�%A�  A��/A��A��
A���A��A��A��;A��HA��TA��TA��`A��TA��HA��;A��;A��;A��;A��HA��HA��TA��TA��AɋDA�x�A�`BA�^5A��A�?}A�`BA��A�JA��^A��A�bA��!A�dZA�VA��TA�t�A���A���A���A�\)A�+A�A���A���A���A��mA�C�A�{A��jA��A�ZA�ffA��A�l�A�7LA�%A�p�A���A�jA��A��#A�^5A���A�%A�hsA�E�A��FA��yA��A�-A���A�A��^A�;dA�^5A�^5A�hsA��FA��A��jA�bA�Q�A���A�A�A��A�A��TA���A�I�A��FA��;A�^5A�\)A���A�E�A�hsA�C�A�Q�A��/A�5?A�5?A|v�A{Ay��Axv�Aw��Av�Aq�-An�RAk�
Ag�Af�`Ae��AcdZAb=qAahsA`��A`A_&�A];dAXȴAT�`AR~�AQdZAN�`AK��AH�AF�+AE"�AC��ABn�AA�AAG�A@�9A?�wA?oA>JA;��A9�A9A733A6A5x�A4��A3��A2�A1ƨA0�A0z�A/��A.�A-�mA-&�A,��A,�DA,v�A+VA)�FA)S�A(�A(I�A'�wA'33A&�\A&A&9XA&bNA&^5A%�hA$bNA#`BA#/A"��A"{A r�AoA��A~�A�AVA�\AVA�mA��A��A��A�;A;dA~�A�
Ax�A�A�9A �AdZA-A~�A-A(�A��AM�AoA��A/A
A�A	�#A	7LA�jA��Ar�A9XA�mAhsA�`A5?A��AhsA^5A��AS�A��A9XA��AA �+A �@���@��H@��@��@�ff@�$�@��/@�\)@�ff@���@�@�j@���@��@���@�b@��@�ff@�G�@�K�@�9X@�@�Ĝ@�A�@߅@��@��@�j@���@��@�1'@�l�@��@��@ԃ@��@�\)@�ȴ@�`B@���@�t�@�hs@��/@�bN@�33@�n�@��@�p�@ȣ�@ǍP@���@�K�@�V@�@�Ĝ@�9X@�  @��F@�33@�~�@��@�G�@��@�9X@��
@�+@�^5@��u@��P@�@�ȴ@���@�-@��T@��^@���@�G�@��j@�z�@��;@���@�$�@���@�G�@���@�I�@���@�"�@�{@���@�7L@�V@�I�@��P@�C�@�
=@��T@��@���@�j@��;@���@�@���@�`B@���@�r�@��@���@��7@���@�M�@��\@�(�@�hs@���@��-@���@�I�@�
=@�M�@���@���@��D@�C�@�C�@�t�@�"�@��@�"�@��\@�n�@��@�X@���@�(�@���@���@�S�@�\)@�o@�@�"�@��H@�J@��@��@�V@�A�@�1@���@��w@��;@�  @�1@��@�1@� �@���@���@�^5@���@�G�@�7L@�&�@���@���@� �@��F@��P@�\)@�
=@��H@���@�$�@�p�@�%@��@�|�@�l�@�+@�V@��@�7L@�V@���@��@���@���@��D@�V@�7L@��@�I�@��m@��F@�o@�
=@�33@�"�@�M�@��-@��@��@�G�@��-@��-@�`B@�(�@�1@��@�1@���@���@��@��@��u@��D@��@��@��
@�S�@�
=@��@���@��m@��w@�  @�ƨ@��#@���@���@��H@���@�
=@��@�~�@���@��T@��#@�x�@�|�@�S�@�t�@�t�@���@�J@���@�^5@�ff@�ff@�=q@�-@�^5@�2�@~�@uj@jߤ@`��@[�@U�7@M�@EA @>��@7O@0g8@+
=@%-w@ Ft@��@O@��@v�@�@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A�  A�A�A�%A�  A��/A��A��
A���A��A��A��;A��HA��TA��TA��`A��TA��HA��;A��;A��;A��;A��HA��HA��TA��TA��AɋDA�x�A�`BA�^5A��A�?}A�`BA��A�JA��^A��A�bA��!A�dZA�VA��TA�t�A���A���A���A�\)A�+A�A���A���A���A��mA�C�A�{A��jA��A�ZA�ffA��A�l�A�7LA�%A�p�A���A�jA��A��#A�^5A���A�%A�hsA�E�A��FA��yA��A�-A���A�A��^A�;dA�^5A�^5A�hsA��FA��A��jA�bA�Q�A���A�A�A��A�A��TA���A�I�A��FA��;A�^5A�\)A���A�E�A�hsA�C�A�Q�A��/A�5?A�5?A|v�A{Ay��Axv�Aw��Av�Aq�-An�RAk�
Ag�Af�`Ae��AcdZAb=qAahsA`��A`A_&�A];dAXȴAT�`AR~�AQdZAN�`AK��AH�AF�+AE"�AC��ABn�AA�AAG�A@�9A?�wA?oA>JA;��A9�A9A733A6A5x�A4��A3��A2�A1ƨA0�A0z�A/��A.�A-�mA-&�A,��A,�DA,v�A+VA)�FA)S�A(�A(I�A'�wA'33A&�\A&A&9XA&bNA&^5A%�hA$bNA#`BA#/A"��A"{A r�AoA��A~�A�AVA�\AVA�mA��A��A��A�;A;dA~�A�
Ax�A�A�9A �AdZA-A~�A-A(�A��AM�AoA��A/A
A�A	�#A	7LA�jA��Ar�A9XA�mAhsA�`A5?A��AhsA^5A��AS�A��A9XA��AA �+A �@���@��H@��@��@�ff@�$�@��/@�\)@�ff@���@�@�j@���@��@���@�b@��@�ff@�G�@�K�@�9X@�@�Ĝ@�A�@߅@��@��@�j@���@��@�1'@�l�@��@��@ԃ@��@�\)@�ȴ@�`B@���@�t�@�hs@��/@�bN@�33@�n�@��@�p�@ȣ�@ǍP@���@�K�@�V@�@�Ĝ@�9X@�  @��F@�33@�~�@��@�G�@��@�9X@��
@�+@�^5@��u@��P@�@�ȴ@���@�-@��T@��^@���@�G�@��j@�z�@��;@���@�$�@���@�G�@���@�I�@���@�"�@�{@���@�7L@�V@�I�@��P@�C�@�
=@��T@��@���@�j@��;@���@�@���@�`B@���@�r�@��@���@��7@���@�M�@��\@�(�@�hs@���@��-@���@�I�@�
=@�M�@���@���@��D@�C�@�C�@�t�@�"�@��@�"�@��\@�n�@��@�X@���@�(�@���@���@�S�@�\)@�o@�@�"�@��H@�J@��@��@�V@�A�@�1@���@��w@��;@�  @�1@��@�1@� �@���@���@�^5@���@�G�@�7L@�&�@���@���@� �@��F@��P@�\)@�
=@��H@���@�$�@�p�@�%@��@�|�@�l�@�+@�V@��@�7L@�V@���@��@���@���@��D@�V@�7L@��@�I�@��m@��F@�o@�
=@�33@�"�@�M�@��-@��@��@�G�@��-@��-@�`B@�(�@�1@��@�1@���@���@��@��@��u@��D@��@��@��
@�S�@�
=@��@���@��m@��w@�  @�ƨ@��#@���@���@��H@���@�
=@��@�~�@���@��T@��#@�x�@�|�@�S�@�t�@�t�@���@�J@���@�^5@�ff@�ff@�=q@�-G�O�@�2�@~�@uj@jߤ@`��@[�@U�7@M�@EA @>��@7O@0g8@+
=@%-w@ Ft@��@O@��@v�@�@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB?}B>wB>wB?}B?}B@�BG�BH�BI�BJ�BL�BM�BO�BR�BR�BS�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BS�BS�BR�BR�B\)B`BBiyBk�BjBjBm�Bp�Bp�Bq�Bq�Bt�Bs�Bu�Bv�Bv�By�By�By�Bx�Bz�B{�Bz�B}�B|�B{�Bz�By�Bw�Bs�Bp�Bp�Bu�B}�B}�Bx�Bp�Bl�BhsBq�Bo�BjBaHBS�B;dB.B'�B!�B�B\B��B��B�sB�#B��B��B�FB��B��B�DB�7B�B� B}�B|�By�Br�BZB+B%B
��B
�/B
ɺB
�'B
��B
�{B
�B
{�B
o�B
Q�B
B�B
<jB
1'B
(�B
�B	��B	�TB	��B	��B	��B	��B	�7B	{�B	v�B	u�B	p�B	l�B	\)B	A�B	�B	%B��B�B�5B��B�wB�?B�B��B�B�B��B��B��B��B�{B�B�B|�By�Bw�Bw�Bt�Br�Br�Br�Bw�By�Bv�Bn�Bn�Bk�Bo�B|�B~�Bw�Br�Bq�Bq�Bq�Bp�Bo�Bm�Bp�Bt�By�By�Bw�Bt�Bt�Br�Bp�Bl�BffBe`BcTBbNB`BB^5B]/B\)BYBVBVBR�BQ�BP�BO�BO�BO�BN�BO�BM�BM�BK�BH�BG�BG�BF�BE�BC�BB�BC�BB�BB�BA�BA�B@�B@�B@�B?}B?}B?}B>wB=qB?}B=qB>wB=qB=qB=qB<jB;dB;dB:^B:^B:^B8RB;dB;dB:^B;dB;dB:^B9XB:^B;dB:^B9XB:^B8RB7LB7LB7LB:^B9XB9XB9XB9XB:^B:^B:^B:^B=qB?}B>wB@�B@�BA�BB�BB�BC�BE�BE�BH�BL�BL�BM�BP�BQ�BQ�BR�BT�BXB\)BdZBe`BffBjBjBk�Bl�Bn�Bo�Br�Bt�Bw�By�Bz�B|�B� B�B�1B�=B�=B�=B�PB�PB�VB�VB�\B�hB�hB�uB��B��B��B��B��B��B��B��B��B�B��B��B�B�3B�?B�LB�dB�dBBĜB��B��B�yB�B�B�B�B�B�B�B�B�B��B	B	bB	hB	oB	oB	{B	oB	{B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	,B	0!B	33B	49B	49B	49B	5?B	8RB	@�B	D�B	F�B	I�B	L�B	P�B	P�B	O�B	O�B	N�B	L�B	N�B	O�B	T�B	VB	XB	\)B	_;B	cTB	e`B	iyB	jB	k�B	n�B	p�B	s�B	s�B	r�B	s�B	v�B	t�B	s�B	s�B	t�B	t�B	t�B	u�B	v�B	w�B	y�B	x�B	x�B	x�B	u�B	u�B	v�B	w�B	{�B	~�B	�B	�B	�%B	�JB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�LB	�qB	�wB	�wB	B	ÖB	ÖB	ŢB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�/B	�/B	�B	�
B	�
B	�B	�
B	�B	�
B	�/B	�;B	�BB	�BB	�HB	�TB	�B	��B
�B
VB
�B
!|B
&�B
/�B
9	B
?�B
HB
M�B
S�B
Y�B
]~B
aB
g�B
j�B
o B
t�B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B6�B5�B5�B6�B6�B8B?0B@6BA<BBCBDNBETBG`BJsBJsBKyBLBLBLBLBLBLBLBLBLBLBLBKyBKyBJsBJsBS�BW�B`�BcBbBbBeBh'Bh'Bi-Bi-Bl?Bk9BmFBnLBnLBq^Bq^Bq^BpXBrdBsjBrdBuwBtqBsjBrdBq^BoSBk:Bh(Bh)BmGBuxBuxBpYBh)BdB_�Bi/Bg$BbBX�BK�B2�B%�B{BWB&B�B�|B�RB�BҴB�YB�B��B��B�#B��B��By�Bw�Bu�Bt�BqsBjIBQ�B"�B
��B
�qB
��B
�`B
��B
�gB
�%B
{�B
s�B
gKB
I�B
:?B
4B
(�B
 �B
eB	�B	�B	ǘB	��B	��B	�]B	��B	s�B	n�B	m�B	hdB	dKB	S�B	9MB	TB��B��B�hB�BƦB�EB�B��B��B��B��B��B��B��B��B�MB|�Bz�Bt�Bq�Bo�Bo�Bl�Bj�Bj�Bj�Bo�Bq�Bn�BfnBfnBc[BgtBt�Bv�Bo�Bj�Bi�Bi�Bi�Bh{BguBehBh{Bl�Bq�Bq�Bo�Bl�Bl�Bj�Bh{BdcB^>B]8B[,BZ'BXBVBUBTBP�BM�BM�BJ�BI�BH�BG�BG�BG�BF�BG�BE�BE�BC�B@�B?�B?�B>�B=~B;sB:lB;sB:lB:lB9fB9fB8`B8`B8`B7[B7[B7[B6UB5OB7[B5OB6UB5OB5OB5OB4IB3CB3CB2=B2=B2=B01B3CB3CB2>B3DB3DB2>B18B2>B3DB2>B18B2>B02B/,B/,B/-B2?B19B19B19B19B2?B2?B2?B2?B5RB7^B6XB8dB8dB9jB:pB:pB;wB=�B=�B@�BD�BD�BE�BH�BI�BI�BJ�BL�BO�BT
B\:B]@B^FBb_Bb_BceBdkBfxBg~Bj�Bl�Bo�Bq�Br�Bt�Bw�B|�B�B�B�B�B�/B�/B�5B�5B�;B�FB�FB�SB�eB�kB�xB��B��B��B��B��B��B��B��B��B��B�B�B�)B�AB�AB�kB�xBĩB��B�SB�YB�YB�YB�YB�kB�kB�xB�xB�B�B��B	:B		?B	
FB	
FB	RB	
GB	SB	�B	�B	qB	jB	wB	�B	�B	�B	"�B	#�B	'�B	+	B	,B	,B	,B	-B	0(B	8XB	<qB	>}B	A�B	D�B	H�B	H�B	G�B	G�B	F�B	D�B	F�B	G�B	L�B	M�B	O�B	S�B	WB	['B	]3B	aLB	bRB	cXB	fkB	hwB	k�B	k�B	j�B	k�B	n�B	l�B	k�B	k�B	l�B	l�B	l�B	m�B	n�B	o�B	q�B	p�B	p�B	p�B	m�B	m�B	n�B	o�B	s�B	v�B	y�B	{�B	}�B	�B	�'B	�LB	�^B	�qB	�qB	�qB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�@B	�FB	�FB	�^B	�dB	�dB	�pB	B	ƧB	ƧB	ɺB	ʿB	ƧB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�G�O�B	�WB	��B	�SB
!B
`B
FB
KB
'�B
0�B
7zB
?�B
E�B
KXB
QcB
UFB
X�B
_iB
baB
f�B
l�B
p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170902    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170902  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170902  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                