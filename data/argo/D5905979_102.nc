CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:17Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170917  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               fA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�ݝ���1   @�ݞhK�,@7���l�D�c>��vȴ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    fA   B   B   @���@���A   A   A@  A`  A�  A�33A�  A�  A�  A���A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�33B���B���B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�)D�)D�ffD���D��fD�%D�`RD��
D��HD� RD�U�D��D���D�${D�MDڈ�D��)D��D�^D�RD�˅111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�G�@�z�A=qA>=qA^=qA~=qA�Q�A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw��B�\B�ǮB�ǮB���B�aHB��{B�ǮB�ǮB�ǮB�ǮB���B��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�qC��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�=Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��D\D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DG\DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK�DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`\D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dy�D��D�b�D��>D���D�!�D�\�D���D���D��D�R>D���D��>D� �D�I�DڅqD��D�qD�Z�D��D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��A��TA��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��yA��`A���A��#A���A�hsA�1'A�/A��A٣�A��A�1A�XA���A���AͰ!A��A�K�AȾwAƴ9A�K�A��AāA���A�&�A�ffA��RA��!A�1'A��A�\)A���A��/A��mA�ffA��DA�-A��-A�A��+A��A�O�A�ƨA�ZA��A��9A�&�A�ȴA�5?A�S�A��A��A��wA�7LA�v�A��/A��A�K�A�bA���A���A��A��A�&�A��A�  A��^A���A��#A�dZA���A�\)A��A��A�ƨA�%A��A��A�%A�Q�A�bNA�1A�;dA��wA�oA�-A�ƨA�l�A��A�JA���A��A�7LA��wA���A� �A��A�~�A�C�A��`A�S�A���A��A���A��DA��wA�oA��yA���A�$�A�Q�A���A�=qA�  A�v�A�\)A~�A|5?Ay"�AuG�ArVAn��Am�Ak�7AjĜAj=qAg�TAbbAZ-AW�AV�AV9XAT�`AS��AR9XAO�ANI�AM+AJAI%AG��AE��AD5?AB�jA@�uA>�uA<�RA;p�A8ĜA7A6�A5��A4�yA4Q�A3&�A1l�A0A/K�A.��A+�A)�A(bA'&�A&ĜA&z�A&Q�A& �A%��A#�A!��A�A�+A  Ax�A��A�A�wAA1'A��A�A�A�/A�^A�\A{A�
A`BA�\A�AXA(�AK�A�yA��A1A�hAG�A
��A
��A
ffA
5?A	��A�A�;A
=AJAĜA�/AjAƨA?}Ar�AI�AA�A(�A(�A �AA�A@��R@��m@�ȴ@�$�@��@��@�V@��@�R@��@�33@�V@�|�@�M�@���@���@陚@�Z@��@�(�@��@�@�ƨ@�o@���@ޏ\@���@۝�@���@��m@��m@�M�@Ԭ@�z�@ԓu@Լj@�$�@��@���@ǥ�@�S�@š�@Ų-@ŉ7@�Ĝ@��@¸R@¸R@��@��@��7@�?}@�Q�@���@���@��P@���@�|�@���@���@��;@���@��\@�Ĝ@�bN@��@�S�@�o@��y@���@�ff@���@��@���@��7@�`B@�V@���@�(�@��P@��!@�=q@���@�&�@�z�@��@�=q@���@� �@�(�@�b@�+@��@��@�ȴ@��#@��/@�Q�@�1@�ƨ@���@�o@���@�@��h@��7@�`B@�?}@�G�@���@��P@��@��@���@��!@��!@��R@��R@��+@�M�@�J@��@��h@�G�@�r�@�I�@��@�I�@�I�@�
=@���@���@��y@�;d@�C�@�C�@�33@�dZ@�ƨ@�  @��m@��@��T@��h@���@�5?@�ff@�V@��+@��\@�E�@��T@��h@�$�@�@���@�n�@��T@�hs@��@�I�@��D@���@�@��@��@�Q�@�1'@�ƨ@�+@���@�V@�$�@�@���@���@��@�V@�ff@�@�@���@�`B@��/@��9@���@�b@���@��w@��F@��F@���@�l�@�@��y@��y@���@�33@�"�@�
=@��!@�^5@��@���@��-@��^@��^@���@�p�@�X@�7L@�V@��@��@�Z@�  @��P@�S�@�C�@�
=@���@�n�@�V@�E�@��@�J@��-@�hs@�%@�Ĝ@��D@�z�@�z�@�j@�Z@�A�@�1'@�(�@��@��F@���@��@��P@��@�M�@���@��@t�@l�D@d��@_��@VOv@P?�@I�.@C i@;��@7P�@/�[@*1�@%m]@�Q@33@ѷ@7�@��@!-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��#A��A��TA��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��yA��`A���A��#A���A�hsA�1'A�/A��A٣�A��A�1A�XA���A���AͰ!A��A�K�AȾwAƴ9A�K�A��AāA���A�&�A�ffA��RA��!A�1'A��A�\)A���A��/A��mA�ffA��DA�-A��-A�A��+A��A�O�A�ƨA�ZA��A��9A�&�A�ȴA�5?A�S�A��A��A��wA�7LA�v�A��/A��A�K�A�bA���A���A��A��A�&�A��A�  A��^A���A��#A�dZA���A�\)A��A��A�ƨA�%A��A��A�%A�Q�A�bNA�1A�;dA��wA�oA�-A�ƨA�l�A��A�JA���A��A�7LA��wA���A� �A��A�~�A�C�A��`A�S�A���A��A���A��DA��wA�oA��yA���A�$�A�Q�A���A�=qA�  A�v�A�\)A~�A|5?Ay"�AuG�ArVAn��Am�Ak�7AjĜAj=qAg�TAbbAZ-AW�AV�AV9XAT�`AS��AR9XAO�ANI�AM+AJAI%AG��AE��AD5?AB�jA@�uA>�uA<�RA;p�A8ĜA7A6�A5��A4�yA4Q�A3&�A1l�A0A/K�A.��A+�A)�A(bA'&�A&ĜA&z�A&Q�A& �A%��A#�A!��A�A�+A  Ax�A��A�A�wAA1'A��A�A�A�/A�^A�\A{A�
A`BA�\A�AXA(�AK�A�yA��A1A�hAG�A
��A
��A
ffA
5?A	��A�A�;A
=AJAĜA�/AjAƨA?}Ar�AI�AA�A(�A(�A �AA�A@��R@��m@�ȴ@�$�@��@��@�V@��@�R@��@�33@�V@�|�@�M�@���@���@陚@�Z@��@�(�@��@�@�ƨ@�o@���@ޏ\@���@۝�@���@��m@��m@�M�@Ԭ@�z�@ԓu@Լj@�$�@��@���@ǥ�@�S�@š�@Ų-@ŉ7@�Ĝ@��@¸R@¸R@��@��@��7@�?}@�Q�@���@���@��P@���@�|�@���@���@��;@���@��\@�Ĝ@�bN@��@�S�@�o@��y@���@�ff@���@��@���@��7@�`B@�V@���@�(�@��P@��!@�=q@���@�&�@�z�@��@�=q@���@� �@�(�@�b@�+@��@��@�ȴ@��#@��/@�Q�@�1@�ƨ@���@�o@���@�@��h@��7@�`B@�?}@�G�@���@��P@��@��@���@��!@��!@��R@��R@��+@�M�@�J@��@��h@�G�@�r�@�I�@��@�I�@�I�@�
=@���@���@��y@�;d@�C�@�C�@�33@�dZ@�ƨ@�  @��m@��@��T@��h@���@�5?@�ff@�V@��+@��\@�E�@��T@��h@�$�@�@���@�n�@��T@�hs@��@�I�@��D@���@�@��@��@�Q�@�1'@�ƨ@�+@���@�V@�$�@�@���@���@��@�V@�ff@�@�@���@�`B@��/@��9@���@�b@���@��w@��F@��F@���@�l�@�@��y@��y@���@�33@�"�@�
=@��!@�^5@��@���@��-@��^@��^@���@�p�@�X@�7L@�V@��@��@�Z@�  @��P@�S�@�C�@�
=@���@�n�@�V@�E�@��@�J@��-@�hs@�%@�Ĝ@��D@�z�@�z�@�j@�Z@�A�@�1'@�(�@��@��F@���@��@��P@��G�O�@���@��@t�@l�D@d��@_��@VOv@P?�@I�.@C i@;��@7P�@/�[@*1�@%m]@�Q@33@ѷ@7�@��@!-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�fB	�ZB	�TB	�TB	�5B	�FB	��B	��B
DB
"�B
1'B
A�B
J�B
M�B
\)B
hsB
iyB
l�B
z�B
�uB
�{B
��B
�B
��B
�B
��B
�B
�)B
�yB1B�B"�B6FBD�BT�B_;Bw�B�+B��B��B��B�-B�qBɺB��B�)B�sB��BB\B�B�B)�B-B0!B1'B8RB:^BM�BXBVB[#B\)BdZBdZBaHB_;BbNBdZBjBjBq�Bq�Bk�BaHBXBQ�BG�BI�BA�B6FB�BuB��B�BBĜB�qB�9B�B��B��B�{B�7Bt�B\)BK�BD�B6FB+BDB
��B
�B
�`B
��B
ɺB
�dB
��B
�B
~�B
v�B
^5B
7LB
!�B
DB	�B	�B	��B	�FB	��B	��B	��B	�DB	gmB	8RB	#�B	�B	�B	{B	VB	%B��B�B�B�ZB�/B�B��B��BɺB�}B�?B��B��B��B��B��B��B��B��B��B��B�uB�bB�DB�VB�+B�B�B�B�B�B�B� B�B�B�B�B�B�B�B~�B~�B�B� B�B~�B|�B{�By�Bw�Bv�Bu�Bu�Bt�Br�Bs�Bs�Bs�Br�Br�Bs�Bs�Bs�Br�Br�Br�Bq�Bn�BjBgmBcTBe`BgmBo�Br�Br�Bu�B� B�B�B�B�B�DB��B�{B�oB�bB�7B�%B�B�B�B�B�+B�7B�=B�VB�bB�{B��B�B�B�B�-B�-B�B��B��B��B��B��B�{B�bB�bB�\B�PB�uB�{B�oB��B��B��B��B�B�B�B�B�1B�=B�DB�7B�1B�7B�VB�VB�JB�PB�\B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�?B�LB�RB�XB�dBŢB��B��B��B�B�#B�)B�/B�BB�HB�`B�fB�fB�`B�fB�`B�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	B	%B	1B	
=B	JB	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	%�B	'�B	+B	.B	/B	0!B	6FB	9XB	>wB	G�B	I�B	K�B	M�B	Q�B	W
B	[#B	]/B	\)B	[#B	ZB	[#B	^5B	bNB	dZB	e`B	gmB	gmB	gmB	gmB	jB	r�B	v�B	u�B	t�B	v�B	v�B	{�B	}�B	}�B	w�B	t�B	t�B	w�B	z�B	y�B	w�B	v�B	v�B	w�B	w�B	x�B	y�B	{�B	~�B	� B	�B	�B	�%B	�%B	�%B	�+B	�1B	�DB	�DB	�PB	�PB	�PB	�VB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�!B	�-B	�3B	�FB	�LB	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�}B	��B	B	ÖB	ÖB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�:B	��B	��B
JB
�B
5B
$�B
1AB
9>B
BuB
F�B
N"B
R�B
ZB
]�B
`B
dZB
i�B
m�B
r�B
vF111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ԨB	��B	�BB	�AB
�B
@B
'�B
7�B
A.B
D@B
R�B
^�B
_�B
b�B
qLB
��B
��B
�'B
�vB
�^B
��B
�9B
�iB
ҎB
��B
��BB1B,�B:�BK[BU�Bn*B}�B��B��B�/B��B��B�B�4B�}B��B�3B�kB�B�BB KB#]B&pB'vB.�B0�BD BN]BLQBQpBRvBZ�BZ�BW�BU�BX�BZ�B`�B`�Bg�Bg�Ba�BW�BN_BH<B=�B@
B7�B,�B�B	�B�&B֛B��B��B��B�qB�)B�B��B�BkBR�BB-B;B,�B!kB�B
�NB
�B
��B
�nB
�+B
��B
�EB
z�B
uqB
mAB
T�B
-�B
JB
�B	�"B	УB	�B	��B	�zB	�VB	�1B	��B	]�B	.�B	mB	TB	=B	B	�B��B�B�KB�9B��B��BкBɐB�lB�YB�B��B��B�zB�zB�PB�7B�1B�+B�,B�2B�&B�B�B��B��B}�B{�Bz�By�Bx�Bx�Bw�Bv�Bw�Bz�By�Bw�Bw�Bw�Bw�Bu�Bu�Bw�Bv�Bw�Bu�Bs�Br�Bp�BnzBmtBlnBloBkhBi\BjbBjbBjbBi\Bi\BjbBjcBjcBi]Bi]Bi]BhWBeEBa-B^BZB\B^BfLBi^Bi^BlqBv�Bw�Bw�Bx�Bz�B��B�2B�'B�B�B�B|�B{�Bz�By�B{�B}�B�B��B�B�B�(B�eB��B��B��B��B��B��B��B��B�ZB�;B�5B�)B�B�B�B��B�$B�*B�B�6B�UB�sB�By�Bz�Bx�B{�B~�B��B��B�B~�B�B�B�B��B�B�B�2B�=B�JB�hB�nB��B�{B�uB�bB�nB��B��B��B��B��B��B��B�B�B�B�PBņBǒBʥB��B��B��B��B��B��B�B�B�B�B�B�B�B�%B�1B�DB�=B�=B�=B�VB�oB�uB�uB�{B�{B�B��B��B��B��B��B��B��B	 �B	�B	B		B	
B	%B	+B	7B	UB	[B	aB	gB	nB	tB	zB	�B	�B	!�B	$�B	%�B	&�B	,�B	0 B	5B	>UB	@aB	BmB	DyB	H�B	M�B	Q�B	S�B	R�B	Q�B	P�B	Q�B	T�B	X�B	Z�B	\B	^B	^B	^B	^B	a#B	iTB	mlB	lfB	k`B	mlB	mmB	r�B	t�B	t�B	nsB	k`B	k`B	nsB	q�B	pB	nsB	mmB	mmB	nsB	nsB	oyB	pB	r�B	u�B	v�B	y�B	{�B	|�B	|�B	|�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�<B	�HB	�YB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�"B	�.B	�5B	�5B	�AB	�GB	�GB	�GB	�MB	�SB	�YB	�`B	�`B	�`B	�fB	�fG�O�B	��B	�qB	�}B
�B
"B
�B
xB
'�B
/�B
9B
=[B
D�B
I<B
P�B
TdB
V�B
Z�B
`�B
dxB
i}B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170917    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170917  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170917  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                